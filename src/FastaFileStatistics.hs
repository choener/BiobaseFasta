
-- |

module Main where

import           Conduit
import qualified Data.ByteString as BS
import           System.Console.CmdArgs
import           System.IO (stdin)
import           Text.Printf

import           Biobase.Fasta



data Options = Options
  { infile  :: String
  , size    :: Int
  }
  deriving (Show,Data,Typeable)

-- | Read a file as a simple raw bytestring and create the enhanced suffix
-- array. Note: the input is a strict bytestring. We need to load the
-- complete bytestring anyway, so we make it strict. We don't mmap because
-- the whole string is loaded.

options = Options
  { infile  =  def &= help ""
  , size    = 1000 &= help "maximal chunked block size"
  }

stats !k (StreamHeader _ _)  = k
stats !k (StreamFasta x _ _) = k + BS.length x

main = do
  o <- cmdArgs options
  case o of
    Options{..} -> do ml <- runResourceT $ if null infile
                                            then sourceHandle stdin =$= sizedStreamEvent size $$ foldlC stats 0
                                            else sourceFastaFile size infile $$ foldlC stats 0
                      print ml

