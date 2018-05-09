
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
streamingFasta (HeaderSize hSz) oSz (CurrentSize cSz) f = go (FindHeader [] 0) where
  go (FindHeader hdr cnt) = \case
    Empty retVal → do
      unless (P.null hdr) $ return () -- TODO handle last *EMPTY* fasta? (only if there is no content will this happen)
      SI.Return retVal -- return $ error "Empty retval"
    Go m → SI.Effect $ liftM (go (FindHeader hdr cnt)) m
    Chunk b bs
      | Nothing ← mk → if cnt > hSz
                        then go (FindHeader hdr cnt) bs
                        else go (FindHeader (b':hdr) (BS.length b' + cnt)) bs
      | Just k  ← mk → go (HasHeader (BS.take hSz $ BS.concat $ P.reverse $ BS.take k b':hdr) [] 0)
                          (Chunk (BS.drop (k+1) b') bs)
      where b' = if P.null hdr then BS.dropWhile (\c → c/='>' && c/=';') b else b
            mk = BS.elemIndex '\n' b'
  go (HasHeader hdr cs cnt) = \case
    Empty retVal → do
      f (Header hdr) (Overlap BS.empty) (Current (BS.concat $ reverse cs) 0)
      SI.Return retVal
    Go m → SI.Effect $ liftM (go (HasHeader hdr cs cnt)) m
    Chunk b bs → case newFastaIndex b of
      Nothing → let (this,next) = BS.splitAt (cSz-cnt) $ BS.filter (/= '\n') b
                in  if BS.length this + cnt >= cSz
                    then do f (Header hdr) (Overlap BS.empty) (Current (BS.concat $ reverse cs) 0)
                            go (HasHeader hdr [] 0) $ Chunk next bs
                    else go (HasHeader hdr (this:cs) (BS.length this + cnt)) (if BS.null next then bs else Chunk next bs)
      Just new
        | new > 0 → let (this,next) = BS.splitAt new b
                    in  go (HasHeader hdr cs cnt) $ Chunk this (Chunk next bs)
        | otherwise → do f (Header hdr) (Overlap BS.empty) (Current (BS.concat $ reverse cs) 0)
                         go (FindHeader [] 0) $ Chunk b bs
  newFastaIndex b = getMin <$> (Min <$> BS.elemIndex '>' b) SG.<> (Min <$> BS.elemIndex ';' b)

data FindHeader
  = FindHeader [BS.ByteString] Int
  | HasHeader  BS.ByteString [BS.ByteString] Int

{-
streamingFasta hSz oSz cSz f
  = concats
  . maps (handleFastaEntry hSz (overlappedFasta oSz cSz f))
  . streamFastaLines
  . S8.lines

-- | Handle a single Fasta entry. A window of @CurrentSize@ is moved over the
-- entry, with overlap from the previous window being carried over.
--
-- TODO It would be possible to have an overlap size greater than current size,
-- but is this in any way useful?

overlappedFasta
  ∷ forall m w r a
  . ( Monad m )
  ⇒ OverlapSize
  -- ^ Size of the overlap carried over from the previous window.
  → CurrentSize
  -- ^ Size of the current window.
  → (Header w → Overlap w → Current w → Stream (Of a) m ())
  -- ^ Function that does the actual computation on a @Current@ window,
  -- producing a stream of @a@ values.
  → Header w
  -- ^ Header of this Fasta entry.
  → ByteString m r
  -- ^ Incoming Fasta entry, without header.
  → Stream (Of a) m r
  -- ^ Stream of @a@s being produced.
{-# Inlinable overlappedFasta #-}
overlappedFasta (OverlapSize oSz) (CurrentSize cSz) f header = go BS.empty (index 0) where
  go ∷ BS.ByteString → Index 0 → ByteString m r → Stream (Of a) m r
  go _ _ (Empty r) = return r
  go overlap idx s = effect $ do
    current :> rest ← S8.toStrict $ S8.splitAt (fromIntegral cSz) s
    let l = BS.length current - oSz
    return $ if BS.null current
      then go current idx rest
      else f header (Overlap overlap) (Current current idx)
           >> go (BS.drop l current) (Index $ getIndex idx + BS.length current) rest

-- | Handle a streaming Fasta entry. Collapse a Fasta entry based on a stream
-- of lines into the header (with @HeaderSize@), and a single @ByteString m r@.
-- This tuple is handed off to a function to actually handle parsing the Fasta
-- file.

handleFastaEntry
  ∷ forall m w f r
  . ( Monad m, Functor f )
  ⇒ HeaderSize
  -- ^ Transform the streamed in header into a strict bytestring.
  → (Header w → ByteString m r → Stream f m r)
  -- ^ Function that does the actual computation on the full, streaming, data.
  → Stream (ByteString m) m r
  -- ^ Incoming stream of lines for a single Fasta entry.
  → Stream f m r
  -- ^ Resulting stream of functor @f@ which holds the results.
{-# Inlinable handleFastaEntry #-}
handleFastaEntry (HeaderSize hsz) eachFasta s =
  effect $ SI.inspect s >>= \case
    Left l → return $ SI.Return l
    Right r → do
      hdr :> rest ← S8.toStrict $ S8.splitAt (fromIntegral hsz) r
      fasta ← S8.concat <$> S8.effects rest
      return $ eachFasta (Header hdr) fasta

-- | Fully stream incoming Fasta lines. Lines are handled in a streaming
-- fashion as well, meaning that input of very long lines is still handled in
-- constant space.

streamFastaLines
  ∷ forall m r
  . ( Monad m )
  ⇒ Stream (ByteString m) m r
  -- ^ Incoming stream of lines.
  → Stream (Stream (ByteString m) m) m r
  -- ^ Outgoing stream of (stream of lines), where each (stream of lines) is
  -- one Fasta entry.
{-# Inlinable streamFastaLines #-}
streamFastaLines = go where
  go (SI.Return l) = SI.Return l
  go s = SI.Step $ fmap go $ splitFastaLines s

data HasHeader
  = NoHeaderYet
  | HasHeader
  deriving (Eq,Ord,Show)

-- | Split lines of Fasta input before a new Fasta entry begins. This new entry
-- may not begin on the first line.
--
-- This is slightly tricky. If the input is a legal Fasta file, then
-- 'splitFastaLines' splits after the first complete entry, and returns the
-- second and more entries as the return value. The first entry is to be
-- handled in a streaming fashion.
--
-- However, if the first entry is missing the header lines, this "illegal"
-- entry is still parsed in, this time still splitting before the next header
-- line.
--
-- Step through lines, doing at least one step. Stop if a line is found that
-- starts a fasta entry, but is not the first line. Given
-- @
-- >A
-- a
-- >B
-- b
-- @
-- this will handle @>A\na@ but not @>B\nb@. Even with @>A@ missing, it would
-- still handle @a@.

splitFastaLines
  ∷ forall m r
  . ( Monad m )
  ⇒ Stream (ByteString m) m r
  -- ^ Incoming stream of lines.
  → Stream (ByteString m) m (Stream (ByteString m) m r)
  -- ^ Outgoing stream of lines of one Fasta entry; with the remainder in the
  -- return value.
{-# Inlinable splitFastaLines #-}
splitFastaLines = go 1 NoHeaderYet where
  go (k::Int) headerCheck (s ∷ Stream (ByteString m) m r) = SI.effect $ do
    -- anything more in the stream?
    SI.inspect s >>= \case
      -- No, we can't continue at all, provide final return value.
      Left retVal → return . SI.Return $ SI.Return retVal
      -- yes, there is another line
      Right rline → do
        -- next is the bytestring "return from empty"
        mhd :> nextLine ← S8.head (rline ∷ ByteString m (Stream (ByteString m) m r))
        len :> _ ← S8.length rline
        traceShow (k,len,mhd) $
         case mhd of
          -- weird, line with no characters, continue on
          Nothing → return $ go k headerCheck nextLine
          -- one or more lines have been handled and @s@ is a line starting a
          -- new fasta entry. Hence we stop and return @s@ (and all following
          -- lines)
          Just c | headerCheck == HasHeader && (c=='>' || c==';')
            → return $ SI.Return s
          -- continue handling this line, since it belongs to the same fasta
          -- file.
          otherwise → return $ SI.Step $ fmap (go (k+1) NoHeaderYet) rline

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

