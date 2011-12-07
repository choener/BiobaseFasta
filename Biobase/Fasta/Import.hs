
-- An enumeratee for conversion from bytestring to individual FASTA entries is
-- provided. In addition, convenience function for file- and compressed
-- file-loading are available.

module Biobase.Fasta.Import where



-- | Transforms a 'ByteString' into 'Fasta' files.

eneeFASTA :: (Monad m) => Enumeratee ByteString [Fasta] m a



-- * convenience

-- | From an uncompressed file.

fromFile = undefined

-- | From a gzip-compressed file.

fromFileZip = undefined
