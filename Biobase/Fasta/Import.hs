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

type FastaFunction z
  = FastaHeader     -- ^ the ">" header
  -> StartPos       -- ^ where in the original sequence to start
  -> WindowSize     -- ^ how many characters we are looking at
  -> PeekSize       -- ^ this many characters are from the next window (peeking into)
  -> TrailSequence  -- ^ trailing last window-size characters
  -> FastaData      -- ^ the actual sequence data
  -> z              -- ^ and what we return as result

-- | Starting position in FASTA entry.

type StartPos = Int

-- | Current header (the line starting with '>')

type FastaHeader = ByteString

-- | FASTA data

type FastaData = ByteString

-- | Window

type WindowSize = Int

-- | How many characters to peek forward

type PeekSize = Int

-- | Last window-size characters as a bytestring

type TrailSequence = ByteString



-- * conversion from FASTA to data of type 'z'.

-- | Takes a bytestring sequence, applies 'f' to each bytestring of windowsize
-- and returns the results z. The "trail" is a suffix of 'PeekSize' from the
-- previous window.

rollingIter
  :: (Monad m, Functor m, Nullable z, Monoid z)
  => (StartPos -> WindowSize -> PeekSize -> TrailSequence -> FastaData -> z)
  -> WindowSize
  -> PeekSize
  -> Enumeratee ByteString z m a
rollingIter f windowSize peekSize = unfoldConvStream go (0,"") where
  go (start,trail) = do
    yss <- roll (windowSize+peekSize) windowSize -- take w+p characters, but drop only w characters from stream
    case yss of
      [ys] -> do let l = BS.length ys -- this is the number of real nucleotides
                 -- keep p characters from window, note that ys == window++peek, hence this construction
                 let newTrail = BS.take peekSize . BS.drop (windowSize-peekSize) $ ys
                 return $ ( ( start + l , newTrail )
                          , f start windowSize peekSize trail ys
                          )
      _ -> error "rollingIter: error"
{-# INLINE rollingIter #-}

-- | Outer enumeratee. See the two convenience functions for how to use it
-- (just like any enumeratee, basically).
--
-- The fasta function 'f' manipulates small stretches of fasta data and has
-- arguments: fasta header, fasta data, start position (all filled by
-- eneeFasta).
--
-- Next we have the window size, how many characters to read at once,
--
-- followed by the the number of characters to read in addition.
--
-- The work is actually done by 'rollingIter'.

eneeFasta
  :: (Monad m, Functor m, Nullable z, NullPoint z, Monoid z)
  => FastaFunction z
  -> WindowSize
  -> PeekSize
  -> Enumeratee ByteString z m a
eneeFasta f windowSize peekSize = unfoldConvStream go "" where
  go hdr = do
    hdr <- I.takeWhile (/=10) -- 10 == '\n'
    is <- joinI
            $   I.breakE (==62) -- 62 == '>'
            ><> I.filter (/=10) -- filter all line breaks for this sequence block
            ><> rollingIter (f hdr) windowSize peekSize
            $   stream2stream
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
{-# INLINE fromFile #-}

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
{-# INLINE fromFileZip #-}
