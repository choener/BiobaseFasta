
module Biobase.Fasta.Conduit where

import           Conduit
import           Control.Arrow ((***))
import           Control.Monad (unless)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit.Zlib (ungzip,gzip)
import           Data.Foldable (toList)
import           Data.List (isSuffixOf)
import           Data.Monoid (mappend)
import qualified Data.ByteString.Char8 as BS

import           Biobase.Fasta.Types



-- | Open a fasta file for reading. If the suffix is @.gz@, all data is
-- decompressed via @ungzip@.

sourceFastaFile :: (MonadResource m) => Int -> FilePath -> Producer m StreamEvent
sourceFastaFile csize fp
  | ".gz" `isSuffixOf` fp = sourceFile fp =$= ungzip =$= sizedStreamEvent csize =$= addStreamHeader
  | otherwise             = sourceFile fp            =$= sizedStreamEvent csize =$= addStreamHeader
{-# Inline sourceFastaFile #-}

-- | Write to a fasta file. If the suffix is @.gz@, all data is compressed
-- via @gzip@.

sinkFastaFile :: (MonadResource m) => Int -> FilePath -> Consumer StreamEvent m ()
sinkFastaFile width fp
  | ".gz" `isSuffixOf` fp = unStreamEvent width =$= gzip =$= sinkFile fp
  | otherwise             = unStreamEvent width          =$= sinkFile fp
{-# Inline sinkFastaFile #-}

-- | Remove StreamHeader events

removeHeaderEvents :: Monad m => Conduit StreamEvent m StreamEvent
removeHeaderEvents = filterC f
  where f StreamHeader{} = False
        f _              = True
{-# Inline removeHeaderEvents #-}

-- | Adds the @streamHeader@ to each fasta element

addStreamHeader :: Monad m => Conduit StreamEvent m StreamEvent
addStreamHeader = await >>= loop BS.empty
  where loop hdr Nothing = return ()
        loop _   (Just x@(StreamHeader hdr _)) = yield x                     >> await >>= loop hdr
        loop hdr (Just x@StreamFasta{})        = yield x{streamHeader = hdr} >> await >>= loop hdr
{-# Inline addStreamHeader #-}

-- | Fills 'prevStreamFasta'.

addPrevStreamFasta :: Monad m => Conduit StreamEvent m StreamEvent
addPrevStreamFasta = await >>= loop BS.empty
  where loop prv Nothing = return ()
        loop prv (Just x@StreamFasta{}) = yield x { prevStreamFasta = prv } >> await >>= loop (streamFasta x)
        loop prv (Just x) = yield x >> await >>= loop BS.empty
{-# Inline addPrevStreamFasta #-}

-- | Returns line-based @StreamEvents@, with the line length the same as in
-- the data.
--
-- The overhead of @streamEvent@ should be negligible (@<10%@ slower than
-- pure @linux wc@).

streamEvent :: Monad m => Conduit ByteString m StreamEvent
streamEvent = linesUnboundedAsciiC =$= start
  where start = await >>= loop 1 1
        loop _    _     Nothing   = return ()
        loop line atoms (Just x)
          | BS.null x             = await >>= loop (line+1) atoms
          | BS.head x `elem` ">;" = yield (StreamHeader x (LineInfo line 1 line (BS.length x) 0)) >> await >>= loop (line+1) 1
          | otherwise             = yield (StreamFasta x BS.empty (LineInfo line 1 line (BS.length x) atoms) BS.empty) >> await >>= loop (line+1) (atoms + BS.length x)
{-# Inline streamEvent #-}

-- | Collapses chunks of @StreamEvent@s in a manner that each collapsed
-- chunk is larger than @csize@, but as small as possible, without breaking
-- up original lines.
--
-- Incurs an overhead of approx @20%@ on top of @streamEvent@.

minSizedStreamEvent :: Monad m => Int -> Conduit StreamEvent m StreamEvent
minSizedStreamEvent csize = start
  where start = await >>= loop []
        loop buf Nothing = emit buf
        loop buf (Just sh@(StreamHeader{})) = emit buf >> yield sh >> await >>= loop []
        loop buf (Just sf@(StreamFasta{}))
          | csize < totlen = await >>= loop (sf:buf)
          | otherwise      = emit (sf:buf) >> await >>= loop []
          where totlen = sum $ map (BS.length . streamFasta) (sf:buf)
        emit []  = return ()
        emit buf = let fub = reverse buf
                       (LineInfo fl fc _  _  fa) = streamLines $ head fub
                       (LineInfo _  _  ll lc _ ) = streamLines $ head buf
                   in  yield (StreamFasta (BS.concat $ map streamFasta fub) BS.empty (LineInfo fl fc ll lc fa) BS.empty)
{-# Inline minSizedStreamEvent #-}

-- | Create stream events with chunked size @csize@. The size hint is
-- honored exactly, except for the last event.
--
-- Significant overhead of @>200%@ compared to @streamEvent@.
--
-- TODO should be a conduit from streamevents to streamevents, like
-- @minSizedStreamEvent@.

sizedStreamEvent :: Monad m => Int -> Conduit ByteString m StreamEvent
sizedStreamEvent csize = linesUnboundedAsciiC =$= start
  where start  = await >>= loop 1 1 []
        -- no more lines, check buffer
        loop _ _ buf Nothing
          | null buf  = return ()
          | otherwise = emitFulls buf >>= emitRemainder
        loop !line !ix !buf (Just x)
          | BS.null x             = await >>= loop (line+1) ix buf
          | BS.head x `elem` ">;" = do emitFulls buf >>= emitRemainder
                                       yield (StreamHeader x (LineInfo line 1 line (BS.length x) 1))
                                       await >>= loop (line+1) 1 []
          | otherwise             = do buf' <- emitFulls ((x , LineInfo line 1 line (BS.length x) ix) : buf)
                                       await >>= loop (line+1) (ix + BS.length x) buf'
        -- will yield StreamEvents until no full element is left
        emitFulls xs
          | null xs       = return xs
          | csize > totln = return xs
          | not (null ts) = let hsl               = sum $ map snd hs
                                ((u,lnfo),l) : us = ts
                                (uh,ut)           = BS.splitAt (csize - hsl) u
                                x                 = BS.concat $ uh : (map (fst . fst) hs)
                                LineInfo fl fc tl tc c = lnfo
                                (_,LineInfo fl' fc' _ _ c') : _ = sx
                            in  do yield $ StreamFasta x BS.empty (LineInfo fl' fc' tl (tc - BS.length ut) c') BS.empty
                                   emitFulls $ if BS.null ut then map fst (reverse us) else reverse ((ut,LineInfo fl (fc + BS.length uh) tl tc (c+BS.length uh)) : map fst us)
          | otherwise     = return xs
          where sx      = reverse xs
                (hs,ts) = span ((< csize) . snd) $ zip sx (scanl1 (+) $ map (BS.length . fst) sx)
                totln   = sum $ map (BS.length . fst) xs
        -- will emit a remainder element. there should be no more full
        -- elements at this point
        emitRemainder xs
          | null xs   = return ()
          | otherwise = let sx = reverse xs
                            x = BS.concat . toList $ fmap fst sx
                            (_,LineInfo fl fc _ _ c) : _ = sx
                            (_,LineInfo _ _ tl tc _) : _ = xs
                        in  yield $ StreamFasta x BS.empty (LineInfo fl fc tl tc c) BS.empty
{-# Inline sizedStreamEvent #-}

-- | Write events back to disk as a fasta file. Want to know the number of
-- columns in the file

unStreamEvent :: Monad m => Int -> Conduit StreamEvent m ByteString
unStreamEvent width = start =$= unlinesAsciiC
  where start = await >>= loop
        loop Nothing = return ()
        loop (Just (StreamHeader x   _)) = yield x >> await >>= loop
        loop (Just (StreamFasta  x p i h))
          | BS.length x >= width = yield hd >> loop (Just $ StreamFasta tl p i h)
          | otherwise            = do mx <- await
                                      case mx of
                                        Nothing                     -> unless (BS.null hd) $ yield x
                                        Just (StreamFasta x' _ _ _) -> loop (Just $ StreamFasta (x `mappend` x') p i h)
                                        Just (StreamHeader x'  _)   -> yield x >> yield x' >> await >>= loop
          where (hd,tl) = BS.splitAt width x
{-# Inline unStreamEvent #-}



test k = do
  xs <- runResourceT $ sourceFastaFile k "/home/choener/Documents/Workdata/DNA-Protein/physarum-protein.fa" $$ sinkList
  mapM_ print $ take 10 xs

