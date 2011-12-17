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



-- | This is the type of the conversion function from FASTA data to the data
-- 'z'. Make certain that all input is used strictly! BangPatterns are the
-- easiest to do. In order, the function expects the current FASTA header, then
-- a data segment, and finally the starting position of the data segment within
-- the full FASTA data.
--
-- If you need the conversion to run in constant time, do not use the
-- convenience functions and replace the final conversion to a strict stream by
-- your own conversion (or output) function.

type FastaFunction z = FastaHeader -> FastaData -> StartPos -> z

-- | Starting position in FASTA entry.

type StartPos = Int

-- | Current header (the line starting with '>')

type FastaHeader = ByteString

-- | FASTA data

type FastaData = ByteString



-- * conversion from FASTA to data of type 'z'.

-- | Takes a bytestring sequence, applies 'f' to each bytestring of windowsize
-- and returns the results z.

rollingIter
  :: (Monad m, Functor m, Nullable z, Monoid z)
  => (ByteString -> StartPos -> z)
  -> Int
  -> Int
  -> Enumeratee ByteString z m a
rollingIter f windowSize peekSize = unfoldConvStream go 0 where
  go start = do
    yss <- roll windowSize (windowSize+peekSize)
    case yss of
      [ys] -> do let xs = BS.filter (/='\n') ys
                 let l = BS.length xs
                 return $ (start + l, f xs start)
      _ -> error "rollingIter: error"

-- | Outer enumeratee. See the two convenience functions for how to use it
-- (just like any enumeratee, basically).

eneeFasta
  :: (Monad m, Functor m, Nullable z, NullPoint z, Monoid z)
  => (ByteString -> ByteString -> StartPos -> z)
  -> Int
  -> Int
  -> Enumeratee ByteString z m a
eneeFasta f windowSize peekSize = unfoldConvStream go "" where
  go hdr = do
    hdr <- I.takeWhile (/=10) -- 10 == '\n'
    is <- joinI
            $   I.takeUpTo 8192
            ><> I.breakE (==62)
            ><> rollingIter (f hdr) windowSize peekSize
            $   stream2stream -- 62 == '>'
    return (hdr, is)
{-# INLINE eneeFasta #-}



-- * Convenience functions: final data is returned strictly.

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
