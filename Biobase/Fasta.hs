{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | This module is currently home to a preliminary version of indices based
-- on a minimal index of 'Zero' or 'One' (and possibly others).

module Biobase.Fasta where

import Data.Int
import Data.ByteString.Char8 as BS
import Data.Lens.Template

-- | A FastaHeader. Starts with '>'. The '>' character is not preserved by the
-- newtype here!

newtype FastaHeader = FastaHeader {unFastaHeader :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Simple creation of a Fasta header.

mkFastaHeader :: ByteString -> FastaHeader
mkFastaHeader xs
  | ">" `isPrefixOf` xs = FastaHeader . BS.copy . BS.drop 1 $ xs
  | otherwise = error $ "not a fasta header: " ++ BS.unpack xs



-- * Zero- or One-based indexing into sequences.

-- | Just an 'Int64' as an 'Index' newtype. We use phantom types (predefined
-- are 'One' and 'Zero') to specify the starting index.

newtype Index t = Index {unIndex :: Int64}
  deriving (Eq,Ord,Show)

-- | Index operations. Can move the index to the left or right.

class IndexOps t where

  -- | Create an index from an 'Int64'.

  mkIndex :: Int64 -> Index t

  -- | Add some value to an index.

  (.+) :: Index t -> Int64 -> Index t

  -- | Subtract some value from an index.

  (.-) :: Index t -> Int64 -> Index t
  (.-) i k = i .+ negate k

-- | Unsafe version of addition '(.+)'.

unsafePlus :: Index t -> Int64 -> Index t
unsafePlus (Index i) k = Index $ i+k



-- * Instances

-- ** Indeces with minimal index "1".

-- | Phantom type for indices "[1..]"

data One

instance Bounded (Index One) where
  minBound = Index 1
  maxBound = Index maxBound

instance IndexOps One where
  mkIndex k
    | k >= 1 = Index k
    | otherwise = error $ "Index One, index too small: " ++ show k
  (.+) (Index i) k
    | ik >= 1   = Index $ ik
    | otherwise = error $ "move: " ++ show (i,k)
    where ik = i+k



-- ** Indices with minimal index "0".

-- | Phantom type for indices "[0..]"

data Zero

instance Bounded (Index Zero) where
  minBound = Index 0
  maxBound = Index maxBound

instance IndexOps Zero where
  mkIndex k
    | k >= 0 = Index k
    | otherwise = error $ "Index Zero, index too small: " ++ show k
  (.+) (Index i) k
    | ik >= 0   = Index $ ik
    | otherwise = error $ "move: " ++ show (i,k)
    where ik = i+k



-- * Some type aliases

-- | Whole scaffolds are indexed starting at one.

type FastaIndex = Index One

-- | Returns a FASTA entry. We have a header, the first index, and the data or
-- payload. In addition, we have "past data", which is the data from the
-- previous window.

data Fasta = Fasta
  { _fastaHeader  :: FastaHeader
  , _firstIndex   :: FastaIndex
  , _fastaData    :: ByteString
  , _pastData     :: ByteString
  , _futureData   :: ByteString
  }
  deriving (Eq,Show)

$( makeLens ''Fasta )

