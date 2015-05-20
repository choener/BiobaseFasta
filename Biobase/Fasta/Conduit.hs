
module Biobase.Fasta.Conduit where

import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import           Data.Char (ord)
import           Data.Word (Word8)



-- | Stream a @Fasta@ file in constant space, independent of the line
-- length in the @Fasta@ file. The first argument is the function to
-- transform a single @Fasta@ entry.
--
-- If the @Fasta@ entry contains comments, only the first comment is made
-- available, and only if it starts directly after the @Fasta@ header.
--
-- Internal to inner handler, no whitespaces are available, i.e. the
-- @Fasta@ file is given as a single stream.

foldFastaSequences
  :: (Monad m)
  => (FastaHeader -> ConduitM ByteString o m a)
  -> ConduitM ByteString o m a
foldFastaSequences f = loop
  where
    loop = do
      a <- CB.takeWhile (/= c2w '\n') =$= do
        a <- f undefined
        CL.sinkNull
        return a
      CB.drop 1
      loop

-- | 

type FastaHeader = (ByteString,ByteString)

-- |

c2w :: Char -> Word8
c2w = fromIntegral . ord

