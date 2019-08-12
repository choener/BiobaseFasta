
module Main where

import Control.Monad.IO.Class
import Data.ByteString.Char8 as BS
import Data.ByteString.Streaming as BSS
import Options.Applicative
import Streaming as S
import Streaming.Prelude as SP
import System.IO (stdin)

import Biobase.Fasta.Streaming as FS



data Options
  -- Extract all sequences that have "header" as "infix".
  = Extract
    { header  ∷ String
    , from    ∷ Int
    , to      ∷ Int
    }

options ∷ Parser Options
options
  = Extract
  <$> strOption (long "header" <> short 'h' <> help "header infix to grep")
  <*> option auto (long "from" <> short 'f' <> help "first nucleotide in sequence")
  <*> option auto (long "to" <> short 't' <> help "last nucleotide in sequence")

-- | Extract a fasta piece from a larger fasta

extract
  ∷ ( Monad m )
  ⇒ String → Int → Int
  → Stream (BSS.ByteString m) m r
  → BSS.ByteString m r
{-# Inlinable extract #-}
extract ifx' f' t' s = BSS.mwrap $ do
  let ifx = BS.pack ifx'
  let f = fromIntegral $ min f' t'
  let t = fromIntegral $ max f' t'
  hdr :> dta ← extractHeader Nothing s
  if ifx `BS.isInfixOf` hdr
  -- we actually have a stream to return
  then return $ do
    BSS.fromStrict hdr
    BSS.drained . BSS.splitAt (t-f-1) . BSS.drop (f-1) $ BSS.concat dta
  -- just drain this stream
  else mapsM_ BSS.effects dta >>= return . return

main ∷ IO ()
main = do
  case undefined of
    Extract hdr f t → BSS.stdout . BSS.concat . maps (extract hdr f t) . FS.streamedFasta $ BSS.stdin

