
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH



-- | Read the three example fasta files as lazy bytestrings. Read them with
-- a small chunk size as well. Export them to a lazy bytestring. Compare
-- the results.
--
-- TODO check if the header and data comes out ok in each case.

fff = undefined



main :: IO ()
main = $(defaultMainGenerator)

