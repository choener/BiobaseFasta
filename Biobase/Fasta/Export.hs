-- | Fasta export

module Biobase.Fasta.Export where
import Biobase.Fasta.Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Data.List

instance Show Fasta where
  show (Fasta _header _sequence) =
    ">" ++ (B.unpack _header) ++ "\n" ++ (B.unpack _sequence) ++ "\n"
