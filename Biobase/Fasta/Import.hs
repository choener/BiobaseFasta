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
import qualified Data.Conduit.List as CL
import Data.Foldable (toList)
import Data.Sequence as S
import Prelude as P
import Data.Lens.Common

import Biobase.Fasta

import Debug.Trace



-- * Conduit-based streaming FASTA parser.

-- | This Fasta streaming function moves windows of size 'WindowsSize', each
-- being directly adjacent to the next one. To be able to handle data on the
-- boundary between two windows, the previous window is always kept. Finally,
-- the /next/ window is added as the future.
--
-- It is the duty of the user to handle present, past, and future data
-- correctly.

streamFasta :: Monad m => WindowSize -> Conduit ByteString m Fasta
streamFasta (WindowSize wsize)
  | wsize <= 0 = error "window size too small"
  | otherwise  = CB.lines =$= conduitState Nix push close =$= CL.sequence addFuture =$= CL.catMaybes
  where
    push Nix l
      | ">" `BS.isPrefixOf` l = return $ StateProducing
                                           (HaveHeader (mkFastaHeader l) (mkIndex 1) S.empty "" S.empty)
                                           []
      | otherwise             = return $ StateProducing
                                           Nix
                                           []
    push (HaveHeader hdr idx cs past xs) l
      | ">" `BS.isPrefixOf` l = return $ StateProducing
                                           (HaveHeader (mkFastaHeader l) (mkIndex 1) S.empty "" S.empty)
                                           [Fasta hdr idx (BS.concat $ toList xs) "" "" | S.length xs > 0]
      | ";" `BS.isPrefixOf` l = return $ StateProducing
                                           (HaveHeader hdr idx (cs |> l) "" S.empty)
                                           [Fasta hdr idx (BS.concat $ toList xs) "" "" | S.length xs > 0]
      | len <  wsize = return $ StateProducing
                                  (HaveHeader hdr idx cs past (xs |> l))
                                  []
      | len >= wsize = return $ StateProducing
                                  (HaveHeader hdr newidx cs newpast (S.singleton . BS.drop drp $ xsl))
                                  (P.zipWith3 (\i x p -> Fasta hdr (idx .+ i) x p "") [0, int64 wsize ..] rs (past : rs))
      where
        xsl = BS.concat . toList $ xs |> l
        len = P.sum . P.map BS.length . toList $ xs |> l
        drp = P.length rs * wsize
        rs  = wsizeBlocks xsl
        newidx = idx .+ int64 drp
        newpast = P.last rs
    close Nix = return []
    close (HaveHeader hdr idx cs past xs)
      | BS.null xsl = return []
      | otherwise   = return $ (P.zipWith3 (\i x p -> Fasta hdr (idx .+ i) x p "") [0, int64 wsize ..] rs (past : rs))
      where
        rs  = wsizeBlocksClose xsl
        xsl = BS.concat . toList $ xs
    wsizeBlocks xs
      | BS.length xs >= wsize = BS.take wsize xs : wsizeBlocks (BS.drop wsize xs)
      | otherwise = []
    wsizeBlocksClose xs
      | BS.length xs > wsize = error "we should not be here: >wsize is handled in push"
      | BS.null xs = []
      | otherwise = [xs]
    int64 = fromIntegral . toInteger
    addFuture = do
      h <- CL.head
      p <- CL.peek
      case (h,p) of
        (Nothing,_)       -> return $ Nothing
        (Just x, Nothing) -> return $ Just x
        (Just x, Just y)  -> return . Just $ if (x^.fastaHeader == y^.fastaHeader)
                                             then (futureData ^= (y ^. fastaData)) x
                                             else x

-- | The current state of the streaming fasta function.

data StreamFastaState
  = Nix
  | HaveHeader
      FastaHeader
      FastaIndex
      (S.Seq ByteString)
      ByteString
      (S.Seq ByteString)



-- | The window size to use.

newtype WindowSize = WindowSize Int

-- | Step size to use.

newtype StepSize = StepSize Int



{-
test :: IO ()
test = do
  runResourceT $ sourceFile "big.fa" $= sf3 10000 9000 $$ CL.foldM (\_ x -> liftIO $ print x) ()
-}

