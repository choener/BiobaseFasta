
-- | A small tool for Fasta files.
--
-- - Reformats with requested number of columns.

module Main where

import           Conduit
import qualified Data.ByteString as BS
import           System.Console.CmdArgs
import           System.IO (stdin)
import           Text.Printf

import           Biobase.Fasta



data Options
  = Reformat
  { infile  :: String
  , columns :: Int
  }
  deriving (Show,Data,Typeable)

-- | Read a file as a simple raw bytestring and create the enhanced suffix
-- array. Note: the input is a strict bytestring. We need to load the
-- complete bytestring anyway, so we make it strict. We don't mmap because
-- the whole string is loaded.

reformat = Reformat
  { infile  = def &= help ""
  , columns =  70 &= help "number of columns for output"
  } &= help "reformat Fasta file to a set number of columns"

main = do
  o <- cmdArgs $ modes [reformat]
  case o of
    Reformat{..} -> runResourceT $
      if null infile
        then sourceHandle stdin =$= streamEvent =$= unStreamEvent columns $$ stdoutC
        else sourceFastaFile columns infile     =$= unStreamEvent columns $$ stdoutC

