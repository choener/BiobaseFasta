{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Biobase.Fasta.QuickCheck where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Char (isDigit)
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit (GSource, ($=), ($$))
import Data.Conduit.List (consume)
import Data.List (sort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List as CL
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Printf

import Biobase.Fasta
import Biobase.Fasta.Import

import Debug.Trace



-- | Fasta QuickCheck generator. We do not build fasta files directly but
-- rather this generator. This makes it a lot simpler to shrink, if necessary.

data FastaQC = FastaQC
  { blocks :: [(Int,String)]
  }
  deriving (Eq,Show,Read)

instance Arbitrary FastaQC where
  arbitrary = do
    n :: Int <- choose (1,1)
    bs <- forM [1..n] someFasta
    return $ FastaQC
      { blocks = zip [1..] bs
      }
  shrink (FastaQC bs) = [FastaQC (removeNth bs n) | n <- [0 .. length bs -1]]

removeNth [] _ = error "oops"
removeNth (x:xs) 0 = xs
removeNth (x:xs) k = x : removeNth xs (k-1)

-- | Some fasta with an embedded number we want to find again! In 50% of the
-- blocks, we include newlines somewhere in there

someFasta idx = do
  k1 <- choose (10,500)
  k2 <- choose (10,500)
  xs <- vectorOf k1 $ elements "ACGT"
  ys <- vectorOf k2 $ elements "ACGT"
  let number = printf "%08d" idx -- THIS EIGHT
  let zs = xs ++ number ++ ys
  nl <- arbitrary
  if nl
  then insertNewlines zs
  else return $ zs

-- | insert the '\n' character every 10-250 characters.

insertNewlines [] = return []
insertNewlines xs = do
  n <- choose (10,250)
  let (ys,zs) = splitAt n xs
  zsnl <- insertNewlines zs
  return $ ys ++ "\n" ++ zsnl

-- | Parse 'FastaQC' as fasta "file".

fromFastaQC :: Monad m => FastaQC -> GSource m ByteString
fromFastaQC (FastaQC bs) = sourceLbs xs where
  xs  = fromChunks $ hdr : fs
  hdr = pack ">fromFastaQC\n"
  fs  = map pack . lines . concatMap snd $ bs


-- | Build streaming parser

extractNumbers :: Monad m => FastaQC -> m [Int]
extractNumbers fastaqc
  =  fromFastaQC fastaqc
  $= streamFasta (WindowSize 100) (StepSize 50)
  $$ CL.fold (\xs x -> xs ++ extract x) []

extract :: Fasta -> [Int]
extract Fasta{..}
  | Just (num,_) <- d = [num | idlen==8]  -- THIS EIGHT
  | otherwise = []
  where
  (xs,ys) = trace ("\n" ++ doShit ++ "\n") $
            BS.break isDigit _fastaData
  d = BS.readInt ys
  idlen = BS.length . BS.takeWhile isDigit $ ys
  doShit = let (xs,ys) = BS.break isDigit _fastaData in show (xs,BS.length xs, ys, BS.length ys)

-- * properties

-- prop_Equal :: FastaQC -> ByteString
prop_Equal fqc = BS.concat . map (BS.take 50 . _fastaData) $ xs where
  Just xs = fromFastaQC fqc $= streamFasta (WindowSize 100) (StepSize 50) $$ consume

-- | Count the number of characters, as designed and as returned by the
-- streaming fasta system.

prop_NumCharacters :: FastaQC -> Bool
prop_NumCharacters fqc = trace (show (xs,ys)) xs == ys where
  xs = length . filter (/='\n') . concatMap snd . blocks $ fqc
  Just ys = fromFastaQC fqc $= streamFasta (WindowSize 100) (StepSize 50) $$ CL.fold (\acc k -> acc + (BS.length $ BS.take 50 $ _fastaData $ k)) 0

-- | Try to find all embedded numbers

prop_FindAll :: FastaQC -> Bool
prop_FindAll fqc = xs == ys where
  xs = sort . map fst $ blocks fqc
  Just ys = fmap sort . extractNumbers $ fqc



xxx :: FastaQC
xxx = read "FastaQC {blocks = [(1,\"CACCTACTTAAAACGTCGAACATGAGAGATTCCACCCGTACTACCTTGATATCGGCACACATGGAATGTCTTATCCGCATGTGAATCCTGATTTACCCTCCTTAGCTC00000001TAACGCCTTATGCACGACGTTAACGCC\")]}"

bla = "CACCTACTTAAAACGTCGAACATGAGAGATTCCACCCGTACTACCTTGATATCGGCACACATGGAATGTCTTATCCGCATGTGAATCCTGATTTACCCTCCTTAGCTC00000001TAACGCCTTATGCACGACGTTAACGCC"
