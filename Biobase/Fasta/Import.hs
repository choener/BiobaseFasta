{-# LANGUAGE OverloadedStrings #-}

-- An enumeratee for conversion from bytestring to individual FASTA entries is
-- provided. In addition, convenience function for file- and compressed
-- file-loading are available.

module Biobase.Fasta.Import where

import Data.ByteString.Char8 as BS
import Data.Iteratee.Iteratee as I
import Data.Iteratee.ListLike as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
import Data.Iteratee.ZLib
import Prelude as P
import Data.Monoid
import Data.List as L

import Biobase.Fasta



-- | Takes a bytestring sequence, applies 'f' to each bytestring of windowsize and returns the results z.

seqIter
  :: (Monad m, Functor m, Nullable z, Monoid z)
  => (ByteString -> StartPos -> z)
  -> Int
  -> Int
  -> Enumeratee ByteString z m a
seqIter f windowSize peekSize = unfoldConvStream go 0 where
  go start = do
    yss <- roll windowSize (windowSize+peekSize)
    case yss of
      [ys] -> do let xs = BS.filter (/='\n') ys
                 let l = BS.length xs
                 return $ (start + l, f xs start)
      _ -> error "eh?"

type StartPos = Int
type FastaHeader = ByteString
type FastaData = ByteString

type FastaFunction z = FastaHeader -> FastaData -> StartPos -> z

-- | 
--
-- TODO is <- is strict, make lazy and map the hdr info onto each result

eneeFasta
  :: (Monad m, Functor m, Nullable z, NullPoint z, Monoid z)
  => (ByteString -> ByteString -> StartPos -> z)
  -> Int
  -> Int
  -> Enumeratee ByteString z m a
eneeFasta f windowSize peekSize = unfoldConvStream go "" where
  go hdr = do
    hdr <- I.takeWhile (/=10) -- 10 == '\n'
    is <- joinI $ I.breakE (==62) ><> seqIter (f hdr) windowSize peekSize $ stream2stream -- 62 == '>'
    return (hdr, is)



-- * convenience

-- | From an uncompressed file.

fromFile :: (Monoid z, Nullable z) => FastaFunction z -> Int -> Int -> FilePath -> IO z
fromFile ff windowSize peekSize fp
  = run =<< ( enumFile 8192 fp
            . joinI
            . eneeFasta ff windowSize peekSize
            $ stream2stream
            )

-- | From a gzip-compressed file.

fromFileZip :: (Monoid z, Nullable z) => FastaFunction z -> Int -> Int -> FilePath -> IO z
fromFileZip ff windowSize peekSize fp
  = run =<< ( enumFile 8192 fp
            . joinI
            . enumInflate GZipOrZlib defaultDecompressParams
            . joinI
            . eneeFasta ff windowSize peekSize
            $ stream2stream
            )
