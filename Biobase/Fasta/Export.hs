-- | Fasta export

module Biobase.Fasta.Export where
import Biobase.Fasta.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List
import GHC.Int

instance Show Fasta where
  show (Fasta _header _sequence) =
    (B.unpack _header) ++ "\n" ++ (B.unpack _sequence) ++ "\n"

prettyPrintFasta :: Int -> Fasta -> String
prettyPrintFasta number (Fasta _header _sequence) = (B.unpack _header) ++ "\n" ++ (B.unpack sequenceLines) ++ "\n"
  where sequenceSlices = breakByteString number _sequence
        sequenceLines = B.intercalate (B.pack "\n") sequenceSlices

prettyByteStringFasta :: Int -> Fasta -> B.ByteString
prettyByteStringFasta number (Fasta _header _sequence) = _header `B.append` bslinebreak `B.append` sequenceLines `B.append` bslinebreak
  where sequenceSlices = breakByteString number _sequence
        sequenceLines = B.intercalate (B.pack "\n") sequenceSlices
        bslinebreak = B.pack "\n"


breakByteString :: Int -> B.ByteString -> [B.ByteString]
breakByteString number bs
  | B.empty == currentLine = []
  | otherwise = currentLine:(breakByteString number rest)
  where (currentLine,rest) = B.splitAt (fromIntToInt64 number) bs

fromIntToInt64 :: Int -> Int64
fromIntToInt64 = fromIntegral

writeFasta :: FilePath -> [Fasta] -> IO ()
writeFasta filePath fastas = do
  let fastabs = map (prettyByteStringFasta 80) fastas
  let outputbs= B.concat fastabs
  B.writeFile filePath outputbs