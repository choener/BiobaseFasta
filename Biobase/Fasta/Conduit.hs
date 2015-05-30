
module Biobase.Fasta.Conduit where

import           Conduit
import           Control.Arrow ((***))
import           Control.Monad (unless)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit.Zlib (ungzip,gzip)
import           Data.Foldable
import           Data.List (isSuffixOf)
import           Data.Monoid (mappend)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as S

import           Biobase.Fasta.Types



-- | Open a fasta file for reading. If the suffix is @.gz@, all data is
-- decompressed via @ungzip@.

sourceFastaFile :: (MonadResource m) => Int -> FilePath -> Producer m StreamEvent
sourceFastaFile csize fp
  | ".gz" `isSuffixOf` fp = sourceFile fp =$= ungzip =$= streamEvent csize
  | otherwise             = sourceFile fp            =$= streamEvent csize
{-# Inline sourceFastaFile #-}

-- | Write to a fasta file. If the suffix is @.gz@, all data is compressed
-- via @gzip@.

sinkFastaFile :: (MonadResource m) => Int -> FilePath -> Consumer StreamEvent m ()
sinkFastaFile width fp
  | ".gz" `isSuffixOf` fp = unStreamEvent width =$= gzip =$= sinkFile fp
  | otherwise             = unStreamEvent width          =$= sinkFile fp
{-# Inline sinkFastaFile #-}

-- | Create stream events with chunked size @csize@.

streamEvent :: Monad m => Int -> Conduit ByteString m StreamEvent
streamEvent csize = linesUnboundedAsciiC =$= start
  where start  = await >>= loop 1 1 S.empty
        -- no more lines, check buffer
        loop _ _ buf Nothing
          | S.null buf = return ()
          | otherwise  = emitFulls buf >>= emitRemainder
        loop !line !ix buf (Just x)
          | BS.null x             = await >>= loop (line+1) ix buf
          | BS.head x `elem` ">;" = do emitFulls buf >>= emitRemainder
                                       yield (StreamHeader x (LineInfo line 1 line (BS.length x) 1))
                                       await >>= loop (line+1) 1 S.empty
          | otherwise             = do buf' <- emitFulls (buf S.|> (x , LineInfo line 1 line (BS.length x) ix))
                                       await >>= loop (line+1) (ix + BS.length x) buf'
        -- will yield StreamEvents until no full element is left
        emitFulls xs
          | S.null xs                          = return xs
          | ((u,lnfo),l) S.:< us <- S.viewl ts = let (uh,ut) = BS.splitAt (csize - hsl) u
                                                     x = BS.concat $ uh : (toList $ fmap (fst . fst) hs)
                                                     LineInfo fl fc tl tc c = lnfo
                                                     (_,LineInfo fl' fc' _ _ c') S.:< _ = S.viewl xs
                                                 in  do yield $ StreamFasta x BS.empty (LineInfo fl' fc' tl (tc - BS.length ut) c')
                                                        emitFulls $ if BS.null ut then fmap fst us else (ut,LineInfo fl (fc + BS.length uh) tl tc (c+BS.length uh)) S.<| fmap fst us
          | otherwise                          = return xs
          where -- this splits into @hs@ which are together smaller than @csize@ and @ts@ whose first element then is larger
                (hs,ts) = S.spanl ((<csize) . snd) $ S.zip xs (S.scanl1 (+) $ fmap (BS.length . fst) xs)
                hsl     = sum . fmap snd $ hs :: Int
        -- will emit a remainder element. there should be no more full
        -- elements at this point
        emitRemainder xs
          | S.null xs = return ()
          | otherwise = let x = BS.concat . toList $ fmap fst xs
                            (_,LineInfo fl fc _ _ c) S.:< _ = S.viewl xs
                            _ S.:> (_,LineInfo _ _ tl tc _) = S.viewr xs
                        in  yield $ StreamFasta x BS.empty (LineInfo fl fc tl tc c)
{-# Inline streamEvent #-}

-- | Write events back to disk as a fasta file. Want to know the number of
-- columns in the file

unStreamEvent :: Monad m => Int -> Conduit StreamEvent m ByteString
unStreamEvent width = start =$= unlinesAsciiC
  where start = await >>= loop
        loop Nothing = return ()
        loop (Just (StreamHeader x   _)) = yield x >> await >>= loop
        loop (Just (StreamFasta  x p i))
          | BS.length x >= width = yield hd >> loop (Just $ StreamFasta tl p i)
          | otherwise            = do mx <- await
                                      case mx of
                                        Nothing                   -> unless (BS.null hd) $ yield x
                                        Just (StreamFasta x' _ _) -> loop (Just $ StreamFasta (x `mappend` x') p i)
          where (hd,tl) = BS.splitAt width x
{-# Inline unStreamEvent #-}



{-

test k = do
  xs <- runResourceT $ sourceFastaFile k "/home/choener/Documents/Workdata/DNA-Protein/physarum-protein.fa" $$ sinkList
  mapM_ print $ take 10 xs

-}

