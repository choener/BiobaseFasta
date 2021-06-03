
-- | Streaming Fasta handling via the @streaming@ library.
--
-- The functions in here should be streaming in constant memory.
--
-- A typical, slightly complicated is this:
-- @
--  forEach :: forall r . Stream (ByteString m) m r -> m (Stream (Of ()) m r)
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

import Control.Lens hiding (Index,Empty, mapped)
import Control.Monad
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..), MonadResource)
import Data.Semigroup as SG
import Debug.Trace
import GHC.Generics (Generic)
import GHC.TypeLits
import Prelude as P
import qualified Data.ByteString.Char8 as BS
import qualified Streaming.Internal as SI
import Streaming as S
import Streaming.ByteString as BSS
import Streaming.ByteString.Char8 as S8
import Streaming.ByteString.Internal as SBI
import Streaming.Prelude as SP

import Data.ByteString.Streaming.Split

import Biobase.Types.BioSequence
import Biobase.Types.Index.Type
import Biobase.Types.Location
import Biobase.Types.Position
import Biobase.Types.Strand



-- |

streamedFasta :: (Monad m) => ByteStream m r -> Stream (Stream (ByteStream m) m) m r
{-# Inlinable streamedFasta #-}
streamedFasta = S.maps collapseData . streamOfStreamedFasta

-- | Here each individual fasta file will be a stream.
--
-- TODO Once this works, @streamingFasta@ should be @S.concats . streamOfStreamedFasta@ ...

streamOfStreamedFasta
  :: forall m r
  . ( Monad m )
  => ByteStream m r
  -> Stream (Stream (ByteStream m) m) m r
  -- ^ 
{-# Inlinable streamOfStreamedFasta #-}
streamOfStreamedFasta = go . S8.lines where
  go = \case
    SI.Return r -> SI.Return r
    SI.Effect m -> SI.Effect (fmap go m)
    SI.Step fs -> SI.Step (SI.Step (fmap (fmap go . splitFasta) fs))

-- | Given a 'Stream (ByteString m) m r' which is a 'Stream' of @lines@, split
-- off the first @Fasta@ entry.

splitFasta :: (Monad m) => Stream (ByteStream m) m r -> Stream (ByteStream m) m (Stream (ByteStream m) m r)
{-# Inlinable splitFasta #-}
splitFasta = loop False where
  loop hdr = \case
    SI.Return r -> SI.Return (SI.Return r)
    SI.Effect m -> SI.Effect (fmap (loop hdr) m)
    SI.Step bs  -> case bs of
      Empty r -> loop hdr r
      Chunk cs xs
        | BS.null cs -> loop hdr $ SI.Step xs
        | h=='>' || h==';' -> if hdr then SI.Return (SI.Step bs) else SI.Step $ fmap (loop True) bs
        | otherwise -> SI.Step $ fmap (loop True) bs
        where h = BS.head cs
      Go m    -> SI.Effect $ fmap ((loop hdr) . SI.Step) m

-- | Given a stream, roughly like @[BS "Header", BS "Data1", BS "Data2", ...]@
-- create a stream like @[BS "Header", BS "Data"]@. This means that the
-- resulting stream holds exactly two @ByteString@'s.

collapseData :: (Monad m) => Stream (ByteStream m) m r -> Stream (ByteStream m) m r
{-# Inlinable collapseData #-}
collapseData = loop where
  loop = \case
    SI.Return r -> SI.Return r
    SI.Effect m -> SI.Effect (fmap loop m)
    SI.Step bs -> case bs of
      Empty r -> loop r
      Chunk cs xs
        | BS.null cs -> loop $ SI.Step xs
        | h=='>' || h==';' -> SI.Step $ fmap (S.yields . S8.concat) bs
        | otherwise -> SI.Step $ fmap loop bs
        where h = BS.head cs
      Go m    -> SI.Effect $ fmap (loop . SI.Step) m


-- | "Rechunk" a stream of bytestrings.

reChunkBS :: (Monad m) => Int -> Stream (ByteStream m) m r -> Stream (ByteStream m) m r
{-# Inlinable reChunkBS #-}
reChunkBS n = splitsByteStringAt n . S8.concat

-- | Assuming a "rechunked" stream of bytestrings, create sequence windows.

chunksToWindows :: Monad m => SequenceIdentifier w -> Strand -> Stream (ByteStream m) m r -> Stream (Of (Location w FwdPosition (BioSequence ty))) m r
{-# Inlinable chunksToWindows #-}
chunksToWindows seqId s = SP.map go . SP.drop 1 . SP.scan indexed (BS.empty, 0, 0) (\(bs,i,_) -> (bs,i)) . S.mapsM S8.toStrict where
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
  :: (Monad m)
  => Maybe Int
  -> Maybe Int
  -> Maybe Int
    -- ^ desired size or a single huge @Fasta@ entry.
  -> SequenceIdentifier w
  -> Strand
  -> (Stream (ByteStream m) m) r
--  -> Stream (Of (BioSequenceWindow w ty FwdLocation)) m r
  -> Stream (Of (PIS w FwdPosition (BioSequence ty))) m r
{-# Inlinable streamedWindows #-}
streamedWindows withPrefix withSuffix winSz seqId strnd
  = (maybe id attachSuffixes withSuffix)
  . (maybe id attachPrefixes withPrefix)
  . SP.map pis
  . chunksToWindows seqId strnd
  . (case winSz of { Nothing -> collapseData; Just sz -> reChunkBS sz })

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
streamLocationLength = SP.fold (\x w -> x + locLength w) 0 id

-- | As a first function, the header should be extracted from a @Fasta@ stream. Since headers may be
-- malformed / malicious, we make it possible to

extractHeader
  :: (Monad m)
  => Maybe Int
  -> Stream (ByteStream m) m r
  -> m (Of BS.ByteString (Stream (ByteStream m) m r))
{-# Inlinable extractHeader #-}
extractHeader hdrSz =
  let go = case hdrSz of { Nothing -> id; Just sz -> S8.drained . S8.splitAt (fromIntegral sz) }
  in S8.toStrict . go . S8.concat . S.splitsAt 1


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

--readFastaFile :: FilePath -> IO [(BS.ByteString,BS.ByteString,BS.ByteString)]
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
