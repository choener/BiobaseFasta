{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Conduit as C
import Data.Conduit.Binary as C
import Data.Conduit.List as CL
import Prelude hiding (length)
import System.Console.CmdArgs
import System.IO (stdin,stdout)
import Text.Printf
import qualified Data.ByteString.Char8 as B

import Biobase.Fasta.Import

import Control.Monad.IO.Class (liftIO, MonadIO (..))


data Options
  = Info
    { description :: Bool
    , length :: Bool
    }
  deriving (Show,Data,Typeable)

info = Info
  { description = False &= help "print description, not just the indentifier"
  , length = False &= help "print length of data section, too. will be last element."
  }

main :: IO ()
main = do
  o <- cmdArgs $ modes [info &= auto]
  case o of
    (Info d l) -> doInfo d l

doInfo d l = do
  runResourceT
    $  sourceHandle stdin
    $= parseEvents 10000
    $= CL.concatMapAccum (countChars d l) Nothing
    $$ sinkHandle stdout

countChars showD showL = go where
  go h@Header{} Nothing                = ( Just (h,0) , [] )
  go h@Header{} (Just (hold, k))       = ( Just (h,0), [prnt hold k] )
  go (Data d)   Nothing                = ( Just (Header "" "",0), [] )
  go (Data d)   (Just (h, k))          = ( Just (h, k + B.length d), [] )
  go Done       Nothing                = ( Nothing, [] )
  go Done       (Just (h, k))          = ( Nothing, [prnt h k] )
  prnt hdr k = printHeader hdr `B.append` (B.pack $ if showL then printf " %8d" k else "") `B.append` "\n"

