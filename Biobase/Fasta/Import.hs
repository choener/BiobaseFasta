{-# LANGUAGE OverloadedStrings #-}

-- Conduit-based FASTA file format reading. Designed to be used in streaming
-- applications.
--
-- Note that each 'Fasta' entries data is composed of bytestrings that do not
-- necessarily have a starting internal offset of 0.

module Biobase.Fasta.Import where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 as BS
import Data.Conduit.Binary as CB (lines)
import Data.Conduit ((=$=),Conduit)
import Data.Conduit.Util (conduitState, ConduitStateResult(..))
import Data.Foldable (toList)
import Data.Sequence as S
import Prelude as P

import Biobase.Fasta

import Debug.Trace



-- * Conduit-based streaming FASTA parser.

-- | 'streamFasta' processes fasta files using windows of 'WindowSize' and
-- steps of 'StepSize'. We start at index 1 and go forward in steps of
-- 'StepSize', each time returning 'WindowSize' nucleotides, or less if the
-- entry is smaller.

streamFasta :: (Monad m) => WindowSize -> StepSize -> Conduit ByteString m Fasta
streamFasta (WindowSize wsize) (StepSize ssize)
  | ssize > wsize = error $ "step size > window size, would loose data!"
  | otherwise = CB.lines =$= conduitState Nix push close
  where
    push Nix l
      | ">" `BS.isPrefixOf` l = return $ StateProducing (HaveHeader (mkFastaHeader l) (mkIndex 1) S.empty "" S.empty) []
      | otherwise             = return $ StateProducing Nix []
    push (HaveHeader hdr idx cs past xs) l
      | ">" `BS.isPrefixOf` l = return $ StateProducing (HaveHeader (mkFastaHeader l) (mkIndex 1) S.empty "" S.empty) [Fasta hdr idx (BS.concat $ toList xs) "" | S.length xs > 0]
      | ";" `BS.isPrefixOf` l = return $ StateProducing (HaveHeader hdr idx (cs |> l) "" S.empty) [Fasta hdr idx (BS.concat $ toList xs) "" | S.length xs > 0]
      | len <  wsize = do
          return $ StateProducing (HaveHeader hdr idx cs past (xs |> l)) []
      | len >= wsize = do
          return {- . trace (show (">>>",len,drp)) -} $ StateProducing
                    (HaveHeader hdr newidx cs newpast (S.singleton $ BS.drop drp $ xsl))
                    (P.zipWith3 (\i x p -> Fasta hdr (idx .+ i) x p) [0, int64 ssize ..] rs (past : rs) )
      where
        -- the input has grown to window size or more
        xsl = BS.concat . toList $ xs |> l
        -- the length of the input, deliberately not using "xsl" as to not have to pay for BS.concat
        len = P.sum . P.map BS.length . toList $ xs |> l
        drp = P.length rs * ssize
        newidx = idx .+ int64 drp
        -- how many blocks of wsize, each stepping ssize, do we have? (with 100,50, there would be one at (0,99), (49,149) for length=160
        rs = wsizeBlocks xsl
        -- the last full window which becomes the new past
        newpast = P.last rs
    close Nix = return []
    close (HaveHeader hdr idx cs past xs) = {- trace ("\nXXX" ++ show (xs, sum $ P.map BS.length $ toList xs)) $ -} return $ -- [Fasta hdr idx (BS.concat $ toList xs) past]
      (P.zipWith3 (\i x p -> Fasta hdr (idx .+ i) x p) [0, int64 ssize ..] rs (past : rs)) where
        rs = wsizeBlocksClose xsl
        xsl = BS.concat . toList $ xs
    wsizeBlocks xs
      | BS.length xs >= wsize = BS.take wsize xs : wsizeBlocks (BS.drop ssize xs)
      | otherwise = []
    wsizeBlocksClose xs
      | BS.length xs > 0 = BS.take wsize xs : wsizeBlocksClose (BS.drop ssize xs)
      | otherwise = []
    -- helper transform
    int64 = fromIntegral . toInteger

-- | The window size to use.

newtype WindowSize = WindowSize Int

-- | Step size to use.

newtype StepSize = StepSize Int

-- | Internal state, indicating if we have found the first fasta header yet.

data SF3
  = Nix
  | HaveHeader
      FastaHeader
      FastaIndex
      (S.Seq ByteString)  -- comments
      ByteString          -- the past
      (S.Seq ByteString)  -- current string




{-
test :: IO ()
test = do
  runResourceT $ sourceFile "big.fa" $= sf3 10000 9000 $$ CL.foldM (\_ x -> liftIO $ print x) ()
-}
