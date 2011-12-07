
-- An enumeratee for conversion from bytestring to individual FASTA entries is
-- provided. In addition, convenience function for file- and compressed
-- file-loading are available.

module Biobase.Fasta.Import where

import Data.ByteString.Char8 as BS
import Data.Iteratee.Iteratee as I

import Biobase.Fasta



-- | Transforms a 'ByteString' into 'Fasta' files.

eneeFasta :: (Monad m) => Enumeratee ByteString [Fasta] m a
eneeFasta = undefined



-- * convenience

-- | From an uncompressed file.

fromFile = undefined

-- | From a gzip-compressed file.

fromFileZip = undefined
