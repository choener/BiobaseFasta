{-# LANGUAGE BangPatterns #-}

-- Minimal example showing how to use the library. Expects a gzip-compressed
-- FASTA file as 1st argument.

module Main where

import qualified Data.ByteString.Char8 as  BS
import System.Environment

import Biobase.Fasta.Import



ff !hdr !dta !sp = [show $ BS.length dta]

main = do
  as <- getArgs
  xs <- fromFileZip ff 8192 128 (head as)
  mapM_ putStrLn xs

