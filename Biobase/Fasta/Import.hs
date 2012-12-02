{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

-- Conduit-based FASTA file format reading. Designed to be used in streaming
-- applications.
--
-- On parsing, one can choose the chunk size depending on the application. On
-- rendering into bytestrings, the number of columns for each data line can be
-- selected. This should be less than 80.

module Biobase.Fasta.Import where

import Control.Arrow (second)
import Control.Monad.IO.Class (liftIO, MonadIO (..))
import Control.Monad (unless)
import Data.ByteString (ByteString, breakByte, takeWhile, empty, null, uncons)
import Data.Char
import Data.Conduit as C
import Data.Conduit.Binary as C
import Data.Conduit.List as CL
import Prelude as P hiding (null)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Bio.Core.Sequence (Offset(..))

import Biobase.Fasta



-- | Parse from 'ByteString' into 'FastaWindow's with a past.

parseFastaWindows :: Monad m => Int -> Conduit ByteString m FastaWindow
parseFastaWindows wsize = parseEvents wsize =$= CL.concatMapAccum go Nothing where
  go (Header i d) _ = (Just (0,i,d,""), []) -- offset, identifier, description, past
  go (Data x) Nothing = (Just (0,"","",""), [FastaW "" "" 0 x ""])
  go (Data x) (Just (k,i,d,p)) = (Just (k + (fromIntegral $ B.length x), i, d, x), [FastaW i d (Offset k) x p])

-- | Render from 'FastaWindow's into 'ByteString's.

renderFastaWindows :: Monad m => Int -> Conduit FastaWindow m ByteString
renderFastaWindows cols = CL.concatMapAccum go Nothing =$= renderEvents cols where
  go fw Nothing = (Just (_identifier fw), [Header (_identifier fw) (_description fw), Data (_fasta fw)])
  go fw (Just i) = if _identifier fw == i
                     then (Just i, [Data (_fasta fw)])
                     else go fw Nothing

-- | An event is either a FASTA header or a part of a FASTA data stream,
-- chunked into user-defineable pieces.

data Event
  = Header !ByteString !ByteString
  | Data   !ByteString
  deriving (Eq,Show)

-- | Parse from 'ByteString' into 'Event's.

parseEvents :: Monad m => Int -> GInfConduit ByteString m Event
parseEvents wsize = loopU where
  loopU         = awaitE >>= either return goU
  loopH front   = awaitE >>= either (finishH   front) (goH front)
  loopD k front = awaitE >>= either (finishD k front) (goD k front)
  finishH   front r = let final = front empty
                      in  unless (null final) (yield . uncurry Header . second (B.drop 1) . breakByte 32 . B.drop 1 $ final) >> return r
  finishD k front r = let final = front empty
                      in  unless (null final) (yield $ Data final) >> return r
  goU s = case BC.uncons s of
    Just ('>', _) -> goH id s
    Just _        -> goD 0 id s
    Nothing       -> loopU
  goH sofar more = case uncons rpart of
    Just (_, rpart') -> yield (uncurry Header . second (B.drop 1) . breakByte 32 . B.drop 1 $ sofar fpart) >> goU rpart'
    Nothing          -> loopH . B.append $ sofar more
    where (fpart,rpart) = breakByte 10 more
  goD k sofar more
    | Just ('>',_) <- BC.uncons more = let final = sofar empty in unless (null final) (yield $ Data final) >> goU more
    | otherwise = case uncons rpart of
    Just (_, rpart') -> let k' = k + B.length fpart in case k' `compare` wsize of
                          LT -> goD k' (B.append $ sofar fpart) rpart'
                          EQ -> yield (Data $ sofar fpart) >> goU rpart'
                          GT -> let (lpart,rpart) = B.splitAt wsize $ sofar fpart in yield (Data lpart) >> goD 0 id (B.append rpart rpart)
    Nothing -> let k' = k + B.length more in case k' `compare` wsize of
                 LT -> loopD k' . B.append $ sofar more
                 EQ -> yield (Data $ sofar more) >> loopU
                 GT -> let (lpart,rpart) = B.splitAt wsize $ sofar more in yield (Data lpart) >> goU rpart
    where (fpart,rpart) = breakByte 10 more

-- | Render from 'Event's into 'ByteStrings'. 'cols' is the number of
-- characters after which a newline is introduced into the stream. Such
-- newlines are introduced only into 'Data' events.

renderEvents :: Monad m => Int -> Conduit Event m ByteString
renderEvents cols = CL.concatMap go =$= CL.map (`BC.snoc` '\n') where
  go (Header i d) = [BC.concat $ [">",i] ++ (if null d then [] else [" ", d])]
  go (Data xs)    = rows xs -- let rs = rows xs in (P.map (`BC.snoc` '\n') . init $ rs) ++ [last rs]
  rows xs = let (x,xs') = B.splitAt cols xs
            in if B.length xs <= cols
                 then [xs]
                 else x : rows xs'



test :: IO ()
test = do
  let prnt (Header i d) = BC.putStr i >> BC.putStrLn d
      prnt (Data d)     = BC.putStrLn d
  runResourceT $ sourceFile "big.fa" $= parseEvents 1000 $$ CL.foldM (\_ x -> liftIO $ prnt x) ()

