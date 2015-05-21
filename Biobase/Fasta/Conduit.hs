
module Biobase.Fasta.Conduit where



import           Conduit
import           Control.Monad (unless)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit.Zlib (ungzip,gzip)
import           Data.List (isSuffixOf)
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import           Control.Arrow ((***))

import Biobase.Fasta.Types



-- | Open a fasta file for reading. If the suffix is @.gz@, all data is
-- decompressed via @ungzip@.

sourceFasta :: (MonadResource m) => Int -> FilePath -> Producer m StreamEvent
sourceFasta csize fp
  | ".gz" `isSuffixOf` fp = sourceFile fp =$= ungzip =$= streamEvent csize
  | otherwise             = sourceFile fp            =$= streamEvent csize
{-# Inline sourceFasta #-}

-- | Write to a fasta file. If the suffix is @.gz@, all data is compressed
-- via @gzip@.

sinkFasta :: (MonadResource m) => Int -> FilePath -> Consumer StreamEvent m ()
sinkFasta width fp
  | ".gz" `isSuffixOf` fp = unStreamEvent width =$= gzip =$= sinkFile fp
  | otherwise             = unStreamEvent width          =$= sinkFile fp
{-# Inline sinkFasta #-}

-- | Create stream events with chunked size @csize@.

streamEvent :: Monad m => Int -> Conduit ByteString m StreamEvent
streamEvent csize = linesUnboundedAsciiC =$= start
  where start  = await >>= loop (LineInfo 1 1 1 1)
        -- nothing to do at all
        loop _ Nothing = return ()
        -- starting from the top, nothing in buffer
        loop (LineInfo lf cf lt ct) (Just x)
          -- found an empty line
          | BS.null x             = await >>= loop (LineInfo (lf+1) 1 (lf+1) 1)
          -- found a header line
          | BS.head x `elem` ">;" = do yield (StreamHeader x (LineInfo lf 1 lf (BS.length x)))
                                       await >>= loop (LineInfo (lf+1) 1 (lf+1) 1)
          -- have enough bytes to emit stream data; yield and but the
          -- remainder of the chunk into the buffer
          | BS.length x >= csize  = do yield (StreamFasta hd BS.empty (LineInfo lf cf lt (BS.length hd)))
                                       loop (LineInfo lt (BS.length hd +1) lt (BS.length hd +1)) (Just tl)
          -- we don't have enough bytes for a chunk. Is the stream ended?
          -- If so, yield if some bytes are left over. If the stream is not
          -- ended, collect bytes for next chunk.
          | otherwise             = do mx <- await
                                       case mx of
                                         Nothing -> unless (BS.null hd) $ yield (StreamFasta x BS.empty (LineInfo lf cf lt (BS.length x)))
                                         Just x' -> loop (LineInfo lf cf (lt+1) 1) (Just $ x `mappend` x')
          where (hd,tl) = BS.splitAt csize x
{-# Inline streamEvent #-}

-- | Write events back to disk as a fasta file. Want to know the number of
-- columns in the file

unStreamEvent :: Monad m => Int -> Conduit StreamEvent m ByteString
unStreamEvent width = undefined
{-# Inline unStreamEvent #-}



{-

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

-}

