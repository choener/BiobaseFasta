{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | QuickCheck properties for conduit-based Fasta streaming functions.

module Biobase.Fasta.QuickCheck where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Char (isDigit,isAlpha)
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit (GSource, ($=), ($$), GLSink)
import Data.Conduit.List (consume)
import Data.Lens.Common
import Data.List (sort)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List as CL
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Printf

import Biobase.Fasta
import Biobase.Fasta.Import

import Debug.Trace



options = stdArgs {maxSuccess = 1000}

customCheck = quickCheckWithResult options

allProps = $forAllProperties customCheck



-- | Fasta QuickCheck generator. We do not build fasta files directly but
-- rather this generator. This makes it a lot simpler to shrink, if necessary.

data FastaQC = FastaQC
  { blocks :: [(Int,String)]
  }
  deriving (Eq,Show,Read)

instance Arbitrary FastaQC where
  arbitrary = do
    n :: Int <- choose (1,100)
    bs <- forM [1..n] someFasta
    return $ FastaQC
      { blocks = zip [1..] bs
      }

  -- First, sthrink by removing a block, if that succeeds shrink by removing
  -- characters from each block in turn.

  shrink (FastaQC bs)
    | length bs >  1 = remBlock ++ remChars
    | length bs == 1 = remChars
    | null bs        = []
    where
      remBlock = [FastaQC (removeNth bs n) | n <- [0 .. length bs -1]]
      remChars = [FastaQC (removingKchars bs k)
                 | let s = sum . map (length . snd) $ bs
                 , let divs = reverse . takeWhile (>0) . iterate (`div` 2) $ s
                 , k <- divs ++ ([1 .. s] L.\\ divs)
                 ]

-- | removes k characters from the input, unless they are digits. Blocks are
-- removed if only digits remain in a block.

removingKchars [] _ = []
removingKchars ((idx,b):bs) k
  | l < k = (idx,rem k b) : bs
  | all isDigit b' = removingKchars bs (k-l)
  | otherwise = (idx,rem k b) : removingKchars bs (k - l)
  where
    b' = rem k b
    l = length b
    rem i [] = []
    rem i (x:xs)
      | isDigit x = x : rem (i-1) xs
      | i < 0     = xs
      | otherwise = rem (i-1) xs

-- | Removes the nth object from a list.

removeNth [] _ = error "oops"
removeNth (x:xs) 0 = xs
removeNth (x:xs) k = x : removeNth xs (k-1)

-- | Some fasta with an embedded number we want to find again! In 50% of the
-- blocks, we include newlines somewhere in there

someFasta idx = do
  k1 <- choose (10,200)
  k2 <- choose (10,200)
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



-- * properties

-- | Equality of the original and the streamed fastaqc payload.

prop_Equal fqc = xs == (BS.unpack . BS.concat . map _fastaData $ ys) where
  xs = filter (/= '\n') . concatMap snd . blocks $ fqc
  ys = runIdentity $ fromFastaQC fqc $= streamFasta (WindowSize 100) $$ consume

-- | Try to find all embedded numbers: making use of the "past".

prop_EmbeddedNumbers :: FastaQC -> Bool
prop_EmbeddedNumbers fqc
  | xs == ys  = True
  | otherwise = trace (show (xs,ys,zs,runIdentity $ fromFastaQC fqc $= streamFasta (WindowSize 100) $$ consume)) False
  where
    xs = sort . map fst $ blocks fqc
    ys = concat . runIdentity $ fromFastaQC fqc $= streamFasta (WindowSize 100) $= CL.sequence getNumbers $$ consume
    zs = ys L.\\ xs

-- | Finds numbers in the fasta stream.

getNumbers :: Monad m => GLSink Fasta m [Int]
getNumbers = do
  fa <- CL.head
  case fa of
    Nothing -> return []
    Just x  -> do
      let pd = x ^. pastData
      let pdl = BS.length pd
      let fd = x ^. fastaData
      let xs = (BS.drop (pdl-7) pd) `BS.append` fd
      return . map snd . readAllDigits 0 $ xs

-- | Read the digits in 'getNumbers'.

readAllDigits :: Int -> ByteString -> [(Int,Int)]
readAllDigits k xs
  | BS.length ts < 8     = []
  | BS.length dCs < 8    = readAllDigits (k + BS.length is) $ BS.dropWhile isDigit ts
  | Just (d,_) <- maybeD = (k + BS.length is, d) : readAllDigits (k + BS.length is + 8) (BS.dropWhile isDigit ts)
  | otherwise            = []
  where
    (is,ts) = BS.break isDigit xs
    dCs = BS.takeWhile isDigit ts
    maybeD  = BS.readInt ts

