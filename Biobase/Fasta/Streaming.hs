
-- | Streaming Fasta handling via the @streaming@ library.
--
-- The functions in here should be streaming in constant memory.
--
-- TODO Check if this is actually true with some unit tests.

module Biobase.Fasta.Streaming
  ( module Biobase.Fasta.Streaming
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource (runResourceT, ResourceT(..), MonadResource)
import           Data.ByteString.Streaming as BSS
import           Data.ByteString.Streaming.Char8 as S8
import           Data.ByteString.Streaming.Internal (ByteString(..))
import           Data.Semigroup as SG
import           Debug.Trace
import           GHC.TypeLits
import           Prelude as P
import qualified Data.ByteString.Char8 as BS
import qualified Streaming.Internal as SI
import           Streaming as S
import           Streaming.Prelude as SP

import           Biobase.Types.Index.Type



newtype HeaderSize = HeaderSize Int
  deriving (Eq,Ord,Show)

newtype OverlapSize = OverlapSize Int
  deriving (Eq,Ord,Show)

newtype CurrentSize = CurrentSize Int
  deriving (Eq,Ord,Show)

newtype Header (which ∷ k) = Header { getHeader ∷ BS.ByteString }
  deriving (Eq,Ord,Show)

newtype Overlap (which ∷ k) = Overlap { getOverlap ∷ BS.ByteString }
  deriving (Eq,Ord,Show)

-- | Current Fasta window, together with the start index (0-based).

data Current (which ∷ k) = Current { currentFasta ∷ BS.ByteString, currentStart ∷ Index 0 }
  deriving (Eq,Ord,Show)

-- | Fully stream a fasta file, making sure to never exceed a constant amount
-- of memory. The @go@ function yields values of type @a@ down the line for
-- continued streaming.
--
-- @
-- r4 = toList . streamingFasta (HeaderSize 2) (OverlapSize 1) (CurrentSize 2) go . S8.fromStrict $ BS.pack t0
--  where go (Header h) (Overlap o) (Current c) = yield (h,o,c)
-- @

streamingFasta
  ∷ forall m w r a
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
  → (Header w → Overlap w → Current w → Stream (Of a) m ())
  -- ^ The processing function. Takes in the header, any overlap from the
  -- previous window, the current window and produces a stream of @a@s.
  → ByteString m r
  -- ^ A streaming bytestring of Fasta files.
  → Stream (Of a) m r
  -- ^ The outgoing stream of @a@s being processed.
{-# Inlinable streamingFasta #-}
streamingFasta (HeaderSize hSz) (OverlapSize oSz) (CurrentSize cSz) f = go (FindHeader [] 0) where
  -- Find the next FASTA header
  go (FindHeader hdr cnt) = \case
    -- No more data to be had. If There is some part of a header, we will run
    -- the handling function @f@ with empty input. @f@ can decide on how to
    -- handle empty FASTA entries.
    Empty retVal → do
      -- handle case of last empty fasta
      unless (P.null hdr) $ do
        let thisHeader = BS.take hSz $ BS.concat $ P.reverse hdr
        f (Header thisHeader) (Overlap BS.empty) (Current BS.empty 0)
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
      | Just k  ← mk → let thisHeader = BS.take hSz $ BS.concat $ P.reverse $ BS.take k b:hdr
                       in  go (HasHeader thisHeader BS.empty [] 0 0)
                              (Chunk (BS.drop (k+1) b) bytestream)
      where b = if P.null hdr then BS.dropWhile (\c → c/='>' && c/=';') rawBS else rawBS
            mk = BS.elemIndex '\n' b
  -- We actually do have a valid header now and process fasta in parts.
  go hasHeader@(HasHeader hdr overlap cs cnt entries) = \case
    -- No more data, process final input and return.
    Empty retVal → do
      when (cnt>0 || entries==0) $ f (Header hdr) (Overlap BS.empty) (Current (BS.concat $ reverse cs) 0)
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
                            f (Header hdr) (Overlap overlap) (Current thisFasta 0)
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
                         when (cnt>0 || entries==0) $ f (Header hdr) (Overlap overlap) (Current thisFasta 0)
                         go (FindHeader [] 0) $ Chunk b bytestream
  -- Returns the first index (if any) of a new fasta entry symbol.
  newFastaIndex b = getMin <$> (Min <$> BS.elemIndex '>' b) SG.<> (Min <$> BS.elemIndex ';' b)

-- | Control structure for 'streamingFasta'.

data FindHeader
  = FindHeader
      { headerParts ∷ [BS.ByteString]
      -- ^ the collected header parts (in reverse order)
      , headerLength ∷ !Int
      -- ^ accumulated header length
      }
  | HasHeader
      { header ∷ !BS.ByteString
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


{-
t0 = P.unlines
  [ ">Aaaa"
  , "123"
  , ">Bbbb"
  , "4567"
  , ">Cccc"
  , "890"
  ]


r2 = splitFastaLines $ S8.lines $ S8.fromStrict $ BS.pack t0

r3 = streamFastaLines $ S8.lines $ S8.fromStrict $ BS.pack t0

-- r3' ∷ Stream (Stream (Of BS.ByteString) Identity) Identity ()
r3' = toList . mapped toList $ maps (mapped toStrict) r3

r4 = toList . streamingFasta (HeaderSize 2) (OverlapSize 1) (CurrentSize 2) go . S8.fromStrict $ BS.pack t0
  where go (Header h) (Overlap o) (Current c) = yield (h,o,c)
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

