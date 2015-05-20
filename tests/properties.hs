
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH



main :: IO ()
main = $(defaultMainGenerator)

