{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Biobase.Fasta.QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import Text.Printf
import Data.List (sort)

import Biobase.Fasta
import Biobase.Fasta.Import



-- | Fasta QuickCheck generator. We do not build fasta files directly but
-- rather this generator. This makes it a lot simpler to shrink, if necessary.

data FastaQC = FastaQC
  { blocks :: [(Int,String)]
  }
  deriving (Eq,Show)

instance Arbitrary FastaQC where
  arbitrary = do
    n :: Int <- choose (10,1000)
    bs <- forM [1..n] someFasta
    return $ FastaQC
      { blocks = zip [1..] bs
      }
  shrink (FastaQC bs) = [FastaQC (removeNth bs n) | n <- [0 .. length bs -1]]

removeNth [] _ = error "oops"
removeNth (x:xs) 0 = xs
removeNth (x:xs) k = x : removeNth xs (k-1)

-- | Some fasta with an embedded number we want to find again! In 50% of the blocks, we include newlines somewhere in there

someFasta idx = do
  k1 <- choose (10,500)
  k2 <- choose (10,500)
  xs <- vectorOf k1 $ elements "ACGT"
  ys <- vectorOf k2 $ elements "ACGT"
  let number = printf "%08d" idx
  nl <- arbitrary
  if nl
  then undefined
  else return $ xs ++ number ++ ys

prop_FindAll :: FastaQC -> Bool
prop_FindAll fqc = xs == ys where
  xs = sort . map fst $ blocks fqc
  ys = []
