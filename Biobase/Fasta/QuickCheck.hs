{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | QuickCheck properties for conduit-based Fasta streaming functions.
--
-- TODO extend FastaQC to create fasta files with more than one scaffold.

module Biobase.Fasta.QuickCheck where



import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, ByteString)
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Char (isDigit,isAlpha)
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit (GSource, ($=), ($$), GLSink, (=$=))
import Data.Conduit.List (consume)
import Data.Functor.Identity
import Data.List (sort)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (shrinkList)
import Test.QuickCheck.All
import Test.QuickCheck.Instances
import Text.Printf

import Biobase.Fasta
import Biobase.Fasta.Import


import Debug.Trace



-- | Data line of FASTA data

newtype DataQC = DataQC ByteString
  deriving (Show)

instance Arbitrary DataQC where
  arbitrary = do
    n :: Int <- choose (1,100)
    bs <- vectorOf n $ fastaElement
    return . DataQC . pack $ bs
  shrink (DataQC d) = map DataQC $ shrink d

-- |

newtype HeaderIQC = HeaderIQC ByteString
  deriving (Show)

instance Arbitrary HeaderIQC where
  arbitrary = do
    n :: Int <- oneof [return 0, choose (1,30)]
    bs <- vectorOf n $ alphaNum
    return . HeaderIQC . pack $ bs
  shrink (HeaderIQC i) = map HeaderIQC $ shrink i

-- |

newtype HeaderDQC = HeaderDQC ByteString
  deriving (Show)

instance Arbitrary HeaderDQC where
  arbitrary = do
    n :: Int <- oneof [return 0, choose (1,100)]
    bs <- vectorOf n $ alphaNumW
    return . HeaderDQC . pack $ bs
  shrink (HeaderDQC d) = map HeaderDQC $ shrink d

-- |

data HeaderQC = HeaderQC HeaderIQC HeaderDQC
  deriving (Show)

instance Arbitrary HeaderQC where
  arbitrary = liftM2 HeaderQC arbitrary arbitrary
  shrink (HeaderQC i d) = liftM2 HeaderQC (shrink i) (shrink d)

-- |

data FastaQC = FastaQC HeaderQC [DataQC]
  deriving (Show)

instance Arbitrary FastaQC where
  arbitrary = do
    n <- choose (1,100)
    liftM2 FastaQC arbitrary (vectorOf n arbitrary)
  shrink (FastaQC h xs) = liftM2 FastaQC (shrink h) (shrink xs)

newtype MultiFastaQC = MultiFastaQC [FastaQC]
  deriving (Show)

instance Arbitrary MultiFastaQC where
  arbitrary = do
    n <- choose (1,10)
    liftM MultiFastaQC $ vectorOf n arbitrary
  shrink (MultiFastaQC xs) = map MultiFastaQC $ shrinkList shrink xs

fastaElement = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

alphaNum = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['1' .. '9']

alphaNumW = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['1' .. '9'] ++ [' ']

-- |

class ToBS x where
  toBS :: x -> [ByteString]

instance ToBS MultiFastaQC where
  toBS (MultiFastaQC xs) = concat . L.intersperse ["\n"] $ map toBS xs

instance ToBS FastaQC where
  toBS (FastaQC h d) = L.intersperse "\n" $ toBS h ++ concatMap toBS d

instance ToBS HeaderQC where
  toBS (HeaderQC (HeaderIQC i) (HeaderDQC d)) = [ BC.concat $ [">", i] ++ if BC.null d then [] else [" ", d] ]

instance ToBS DataQC where
  toBS (DataQC d) = [d]

-- |

newtype OneBS = OneBS ByteString
  deriving (Show)

instance Arbitrary OneBS where
  arbitrary = do
    x :: MultiFastaQC <- arbitrary
    return . OneBS . BC.concat . toBS $ x

data WR = WR Int Int
  deriving (Show)

instance Arbitrary WR where
  arbitrary = liftM2 WR (choose (10,100)) (choose (10,100))
  shrink (WR w r) = liftM2 WR [w-1 | w>10] [r-1 | r>10]



-- * Properties

options = stdArgs {maxSuccess = 1000}

customCheck = quickCheckWithResult options

allProps = $forAllProperties customCheck



-- ** try to parse / parse - render - parse with different chunk sizes, number
-- of columns.

prop_P_PRP_1 (fqc :: MultiFastaQC, WR w r)
  | xs == ys = True
  | otherwise = trace ("\n\n\n\n\n" ++ (concat $ map show xs ++ ["\n\n"] ++ map show ys)) $ False
  where
    xs = go (parseEvents w)
    ys = go (parseEvents w =$= renderEvents r =$= parseEvents w)
    go f = runIdentity $ sourceLbs (fromChunks . toBS $ fqc) $= f $$ consume

prop_P_PRP_2 (OneBS o, WR w r)
  | xs == ys = True
  | otherwise = trace ("\n" ++ show xs ++ "\n" ++ show ys ++ "\n") $ False
  where
    xs = go (parseEvents w)
    ys = go (parseEvents w =$= renderEvents r =$= parseEvents w)
    go f = runIdentity $ sourceLbs (fromChunks $ [o]) $= f $$ consume



{-
manyS :: [ByteString]
manyS = map (`BC.snoc` '\n')
  [ ">x y"
  , "abc"
  , ">a b"
  , "def"
  ]
-}

