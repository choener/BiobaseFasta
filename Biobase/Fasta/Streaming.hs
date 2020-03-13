
-- | Streaming Fasta handling via the @streaming@ library.
--
-- The functions in here should be streaming in constant memory.
--
-- A typical, slightly complicated is this:
-- @
--  forEach ∷ forall r . Stream (ByteString m) m r → m (Stream (Of ()) m r)
--  forEach dna = do
--    -- extract the header, but at most 123 characters, dropping the rest
--    hdr SP.:> dta ← extractHeader (Just 123) dna
--    -- create windows @ws@ of a particular type. Include the prefix, the suffix, and make each window 10 characters long
--    let ws = (streamedWindows True True (Just 10) (SequenceIdentifier hdr) PlusStrand dta :: SP.Stream (SP.Of (BioSequenceWindow "DNA" DNA 0)) m r)
--    -- count the number of characters in @dna@, get the return value, print each window
--    count SP.:> r ← SP.mapM_ (liftIO . print) . bswSeqLength $ SP.copy ws
--    liftIO $ print count
--    liftIO $ putStrLn ""
--    -- yield one vacuous @()@ result, return the remainder @r@ from dna.
--    return $ SP.yield () *> return r
-- @
--
-- TODO Check if this is actually true with some unit tests.

module Biobase.Fasta.Streaming
  ( module Biobase.Fasta.Streaming
  ) where

import           Control.Lens hiding (Index,Empty, mapped)
import           Control.Monad
import           Control.Monad.Trans.Resource (runResourceT, ResourceT(..), MonadResource)
import           Data.ByteString.Streaming as BSS
import           Data.ByteString.Streaming.Char8 as S8
import           Data.ByteString.Streaming.Internal (ByteString(..))
import           Data.Semigroup as SG
import           Debug.Trace
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Prelude as P
import qualified Data.ByteString.Char8 as BS
import qualified Streaming.Internal as SI
import           Streaming as S
import           Streaming.Prelude as SP

import           Data.ByteString.Streaming.Split

import           Biobase.Types.BioSequence
import           Biobase.Types.Index.Type
import           Biobase.Types.Location
import           Biobase.Types.Position
import           Biobase.Types.Strand

{-

newtype HeaderSize = HeaderSize Int
  deriving (Eq,Ord,Show)

newtype OverlapSize = OverlapSize Int
  deriving (Eq,Ord,Show)

newtype CurrentSize = CurrentSize Int
  deriving (Eq,Ord,Show)

-- | lens into the unique id / first word of the header.

fastaUid ∷ Lens' (SequenceIdentifier w) BS.ByteString
fastaUid = lens getWord updateWord
  where getWord ((BS.words . _sequenceIdentifier) → ws) = case ws of (x:_) → BS.drop 1 x; [] → BS.empty
        updateWord (SequenceIdentifier hdr) w = SequenceIdentifier . BS.unwords $ BS.cons '>' w : tail (BS.words hdr)
{-# Inlinable fastaUid #-}



-- | Fully stream a fasta file, making sure to never exceed a constant amount
-- of memory. The @go@ function yields values of type @a@ down the line for
-- continued streaming.
--
-- @
-- r4 = toList . streamingFasta (HeaderSize 2) (OverlapSize 1) (CurrentSize 2) go . S8.fromStrict $ BS.pack t0
--  where go (Header h) (Overlap o) (Current c) = yield (h,o,c)
-- @

streamingFasta
  ∷ forall m w ty k r a
  . ( Monad m )
  ⇒ HeaderSize
  -- ^ Maximal length of the header. Ok to set to @20 000@, only guards against
  -- an extremely long header line.
  → OverlapSize
  -- ^ How much of the current size to carry over to the next step. Even if set
  -- larger than current size, it will only be at most current size. (But see
  -- todo at 'overlappedFasta')
  → CurrentSize
  -- ^ The size of each window to be processed.
  → ByteString m r
  -- ^ A streaming bytestring of Fasta files.
  → Stream (Of (BioSequenceWindow w ty FwdLocation)) m r
  -- ^ The outgoing stream of @Current@ windows being processed.
{-# Inlinable streamingFasta #-}
streamingFasta (HeaderSize hSz) (OverlapSize oSz) (CurrentSize cSz) = go (FindHeader [] 0) where
  -- Find the next FASTA header
  go (FindHeader hdr cnt) = \case
    -- No more data to be had. If There is some part of a header, we will run
    -- the handling function @f@ with empty input. @f@ can decide on how to
    -- handle empty FASTA entries.
    Empty retVal → do
      -- handle case of last empty fasta
      unless (P.null hdr) $ do
        let thisHeader = BS.take hSz . BS.drop 1 . BS.concat $ P.reverse hdr
        yield $ seqWindow thisHeader BS.empty BS.empty 0
      SI.Return retVal
    -- Effects are wrapped up into a 'Stream' effect.
    Go m → SI.Effect $ liftM (go (FindHeader hdr cnt)) m
    -- We have a chunk of bytestring @rawBS@ with more data in the bytestream
    -- @bs@. We work on @b@, not the @rawBS@. In case we have no header parts
    -- yet, all characters preceeding a fasta header symbol ('>' or ';') are
    -- dropped.
    Chunk rawBS bytestream
      -- No newline in the @b@, hence we add the bytestring to the partial
      -- header, and continue scanning. Note that we add only if we are below
      -- the maximal header size @hSz@ to prevent malicious fasta files from
      -- blowing up memory usage.
      | Nothing ← mk → if cnt > hSz
                        then go (FindHeader hdr cnt) bytestream
                        else go (FindHeader (b:hdr) (BS.length b + cnt)) bytestream
      -- We have found a newline at @k@. Prepare the full header (up to @hSz@
      -- size) and hand over to @HasHeader@ which processes actual fasta
      -- payload.
      | Just k  ← mk → let thisHeader = BS.take hSz . BS.drop 1 . BS.concat . P.reverse $ BS.take k b:hdr
                       in  go (HasHeader thisHeader BS.empty [] 0 0)
                              (Chunk (BS.drop (k+1) b) bytestream)
      where b = if P.null hdr then BS.dropWhile (\c → c/='>' && c/=';') rawBS else rawBS
            mk = BS.elemIndex '\n' b
  -- We actually do have a valid header now and process fasta in parts.
  go hasHeader@(HasHeader hdr overlap cs cnt entries) = \case
    -- No more data, process final input and return.
    Empty retVal → do
      when (cnt>0 || entries==0) . yield $ seqWindow hdr BS.empty (BS.concat $ reverse cs) 0
      SI.Return retVal
    -- Effects to be dealt with.
    Go m → SI.Effect $ liftM (go hasHeader) m
    -- We have incoming data ...
    Chunk b bytestream → case newFastaIndex b of
      -- there is no new fasta starting, meaning that we need to process @b@ as
      -- payload. We split at the maximal size we are allowed according to
      -- @cSz@. If we have hit the limit, we run @f@ on this part of the data
      -- and include the overlap as prefix. Otherwise we continue gathering.
      -- Any newlines are removed from the data.
      Nothing → let (this,next) = BS.splitAt (cSz-cnt) $ BS.filter (/= '\n') b
                in  if BS.length this + cnt >= cSz
                    then do let thisFasta = BS.concat $ reverse $ this:cs
                            yield $ seqWindow hdr overlap thisFasta entries
                            go (HasHeader hdr (BS.drop (BS.length thisFasta - oSz) thisFasta) [] 0 (entries+1))
                               (if BS.null next then bytestream else Chunk next bytestream)
                    else go (HasHeader hdr overlap (this:cs) (BS.length this + cnt) entries)
                            (if BS.null next then bytestream else Chunk next bytestream)
      -- We have a new fasta symbol in @b@. We split at the symbol and re-run
      -- the first part (which will end up being the @Nothing@ case) and put
      -- into @Chunk next bytestream@ the beginning of the next fasta entry.
      -- This part will then be handled by the @otherwise@ case here.
      Just new
        | new > 0 → let (this,next) = BS.splitAt new b
                    in  go (HasHeader hdr overlap cs cnt entries) $ Chunk this (Chunk next bytestream)
        | otherwise → do let thisFasta = BS.concat $ reverse cs
                         -- we only emit on empty @thisFasta@, if there is
                         -- data, or it is the only (then empty) entry.
                         when (cnt>0 || entries==0) . yield $ seqWindow hdr overlap thisFasta entries
                         go (FindHeader [] 0) $ Chunk b bytestream
  -- Returns the first index (if any) of a new fasta entry symbol.
  newFastaIndex b = getMin <$> (Min <$> BS.elemIndex '>' b) SG.<> (Min <$> BS.elemIndex ';' b)
  -- build up a seq-window
  seqWindow hdr pfx seq entries = BioSequenceWindow
    { _bswIdentifier = SequenceIdentifier hdr
    , _bswPrefix = BioSequence pfx
    , _bswInfix  = BioSequence seq
    , _bswSuffix = BioSequence ""
    , _bswInfixLocation = FwdLocation PlusStrand (Index $ entries * cSz) (BS.length seq)
    }

-}

-- |

streamedFasta ∷ (Monad m) ⇒ ByteString m r → Stream (Stream (ByteString m) m) m r
{-# Inlinable streamedFasta #-}
streamedFasta = S.maps collapseData . streamOfStreamedFasta

-- | Here each individual fasta file will be a stream.
--
-- TODO Once this works, @streamingFasta@ should be @S.concats . streamOfStreamedFasta@ ...

streamOfStreamedFasta
  ∷ forall m r
  . ( Monad m )
  ⇒ ByteString m r
  → Stream (Stream (ByteString m) m) m r
  -- ^ 
{-# Inlinable streamOfStreamedFasta #-}
streamOfStreamedFasta = go . S8.lines where
  go = \case
    SI.Return r → SI.Return r
    SI.Effect m → SI.Effect (fmap go m)
    SI.Step fs → SI.Step (SI.Step (fmap (fmap go . splitFasta) fs))

-- | Given a 'Stream (ByteString m) m r' which is a 'Stream' of @lines@, split
-- off the first @Fasta@ entry.

splitFasta ∷ (Monad m) ⇒ Stream (ByteString m) m r → Stream (ByteString m) m (Stream (ByteString m) m r)
{-# Inlinable splitFasta #-}
splitFasta = loop False where
  loop hdr = \case
    SI.Return r → SI.Return (SI.Return r)
    SI.Effect m → SI.Effect (fmap (loop hdr) m)
    SI.Step bs  → case bs of
      Empty r → loop hdr r
      Chunk cs xs
        | BS.null cs → loop hdr $ SI.Step xs
        | h=='>' || h==';' → if hdr then SI.Return (SI.Step bs) else SI.Step $ fmap (loop True) bs
        | otherwise → SI.Step $ fmap (loop True) bs
        where h = BS.head cs
      Go m    → SI.Effect $ fmap ((loop hdr) . SI.Step) m

-- | Given a stream, roughly like @[BS "Header", BS "Data1", BS "Data2", ...]@
-- create a stream like @[BS "Header", BS "Data"]@. This means that the
-- resulting stream holds exactly two @ByteString@'s.

collapseData ∷ (Monad m) ⇒ Stream (ByteString m) m r → Stream (ByteString m) m r
{-# Inlinable collapseData #-}
collapseData = loop where
  loop = \case
    SI.Return r → SI.Return r
    SI.Effect m → SI.Effect (fmap loop m)
    SI.Step bs → case bs of
      Empty r → loop r
      Chunk cs xs
        | BS.null cs → loop $ SI.Step xs
        | h=='>' || h==';' → SI.Step $ fmap (S.yields . S8.concat) bs
        | otherwise → SI.Step $ fmap loop bs
        where h = BS.head cs
      Go m    → SI.Effect $ fmap (loop . SI.Step) m


-- | "Rechunk" a stream of bytestrings.

reChunkBS :: (Monad m) => Int -> Stream (ByteString m) m r -> Stream (ByteString m) m r
{-# Inlinable reChunkBS #-}
reChunkBS n = splitsByteStringAt n . S8.concat

-- | Assuming a "rechunked" stream of bytestrings, create sequence windows.

chunksToWindows :: Monad m => SequenceIdentifier w -> Strand -> Stream (ByteString m) m r -> Stream (Of (Location w FwdPosition (BioSequence ty))) m r
{-# Inlinable chunksToWindows #-}
chunksToWindows seqId s = SP.map go . SP.drop 1 . SP.scan indexed (BS.empty, 0, 0) (\(bs,i,_) → (bs,i)) . S.mapsM S8.toStrict where
  indexed (_,cur,next) bs = (bs,next,next + BS.length bs)
  go (bs,i)
    = Location
        { _locIdentifier = seqId
        , _locPosition   = FwdPosition s (Index i)
        , _locSequence   = BioSequence bs
        }



-- | Make it possible to take a fasta stream and produce a stream of
-- 'BioSequenceWindow's. This is a convenience function around
-- 'withSuffix . withPrefix . chunksToWindows . reChunks'.
--
-- In case of a @Nothing@ window size, a single huge @Fasta@ entry is produced
-- (and materialized!).
--
-- TODO In case of @Nothing@ window size, we use the 'collapseData' function
-- which has one check too many, and will be slightly slower. However, the
-- check should be once per @ByteString@.

streamedWindows
  ∷ (Monad m)
  ⇒ Maybe Int
  → Maybe Int
  → Maybe Int
    -- ^ desired size or a single huge @Fasta@ entry.
  → SequenceIdentifier w
  → Strand
  → (Stream (ByteString m) m) r
--  → Stream (Of (BioSequenceWindow w ty FwdLocation)) m r
  -> Stream (Of (PIS w FwdPosition (BioSequence ty))) m r
{-# Inlinable streamedWindows #-}
streamedWindows withPrefix withSuffix winSz seqId strnd
  = (maybe id attachSuffixes withSuffix)
  . (maybe id attachPrefixes withPrefix)
  . SP.map pis
  . chunksToWindows seqId strnd
  . (case winSz of { Nothing → collapseData; Just sz → reChunkBS sz })

-- | Get the full length of a stream of 'BioSequenceWindow's, counted in
-- characters in each 'bswSequence'.
--
-- To use, start with @bswSeqLength $ SP.copy xs@. Then consume this stream
-- normally. It still provides a 'Stream' of 'BioSequenceWindows's. However,
-- the return type is now not just @r@, but it provides @Int SP.:> r@, where
-- the @Int@ provides the total length of characters within this @Fasta@ entry.
--
-- This value may then be used to fully update negative strand information.

streamLocationLength :: (Monad m, ModifyLocation posTy seqTy) => Stream (Of (Location i posTy seqTy)) m r -> m (Of Int r)
{-# Inlinable streamLocationLength #-}
streamLocationLength = SP.fold (\x w → x + locLength w) 0 id

-- | As a first function, the header should be extracted from a @Fasta@ stream. Since headers may be
-- malformed / malicious, we make it possible to

extractHeader
  :: (Monad m)
  => Maybe Int
  -> Stream (ByteString m) m r
  -> m (Of BS.ByteString (Stream (ByteString m) m r))
{-# Inlinable extractHeader #-}
extractHeader hdrSz =
  let go = case hdrSz of { Nothing -> id; Just sz -> S8.drained . S8.splitAt (fromIntegral sz) }
  in S8.toStrict . go . S8.concat . S.splitsAt 1

{-

foo = S8.fromStrict ">a\na\na\n>b\nb\nb\n"

-- | Control structure for 'streamingFasta'.

data FindHeader
  = FindHeader
      { headerParts ∷ [BS.ByteString]
      -- ^ the collected header parts (in reverse order)
      , headerLength ∷ !Int
      -- ^ accumulated header length
      }
  | HasHeader
      { fhHeader ∷ !BS.ByteString
      -- ^ the (size-truncated) header for this fasta file
      , dataOverlap ∷ !BS.ByteString
      -- ^ overlap (if any) from earlier parts of the fasta file
      , dataParts ∷ [BS.ByteString]
      -- ^ collection of dataParts, in reverse order!
      , dataLength ∷ !Int
      -- ^ total length of data parts, simplifies checking if enough data was collected
      , entries ∷ !Int
      -- ^ count how many entries we have seen
      }

-}

{-
t0 = P.unlines
  [ ">Aaaa"
  , "123"
  , ">Bbbb"
  , "4567"
  , ">Cccc"
  , "890"
  ]


r4 = toList . streamingFasta (HeaderSize 2) (OverlapSize 1) (CurrentSize 2) . S8.fromStrict $ BS.pack t0
-}

{-
--eachFasta (Header h) (Overlap o) (Current c p) = SP.yield (h,o,c)
eachFasta (Header h) (Overlap o) (Current c p) = SP.yield (BS.length h, BS.length o, BS.length c)

--readFastaFile ∷ FilePath → IO [(BS.ByteString,BS.ByteString,BS.ByteString)]
readFastaFile f = do
  let s = 1000000000000
  r ← runResourceT
          $ SP.mapM_ (liftIO . P.print)
          $ streamingFasta (HeaderSize s) (OverlapSize 0) (CurrentSize s) eachFasta
          $ S8.readFile f
  return r
-}

{-
readFastaFile f = do
  let s = 1000000000000
  r ← runResourceT
          $ SP.mapM_ (liftIO . P.print)
          $ SP.mapped S8.toStrict
          $ S8.split '>'
          $ S8.readFile f
  return r
-}
