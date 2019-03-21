
module Main where

import           Prelude as P
import           Data.Functor.Of
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Streaming as BSS
import           Data.ByteString.Streaming.Char8 as S8
import           Data.ByteString.Streaming.Internal (ByteString(..))
import           Streaming as S
import           Streaming.Prelude as SP
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
--import           Test.Tasty.Silver as S
--import           Test.Tasty.Silver.Interactive as SI
import           Test.Tasty.TH
import           Control.Monad.Trans.Resource (runResourceT, ResourceT(..), MonadResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Tasty.Golden as Golden
import           System.FilePath (takeBaseName, replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as BL8

import           Biobase.Fasta.Streaming



main :: IO ()
main = return ()

{-

-- * golden tests

eachFasta (Header h) (Overlap o) (Current c p) = SP.yield (h,o,c)

readFastaFile ∷ FilePath → IO [(BS.ByteString,BS.ByteString,BS.ByteString)]
readFastaFile f = do
  let s = 1000000
  xs :> r ← runResourceT
          $ SP.toList
          $ streamingFasta (HeaderSize s) (OverlapSize 0) (CurrentSize s) eachFasta
          $ S8.readFile f
  return xs

lazyFastaOut = BL8.concat . P.map go
  where go (h,o,c) = BL8.concat
          [ BL8.fromStrict h
          , BL8.pack "\n"
          , BL8.fromStrict o
          , BL8.pack "\n"
          , BL8.fromStrict c
          , BL8.pack "\n"
          ]

goldenTests ∷ IO TestTree
goldenTests = do
  fastaFiles ← Golden.findByExtension [".fa"] "./tests/"
  return $ testGroup "readFastaFile golden tests"
    [ Golden.goldenVsString
        (takeBaseName fastaFile) -- test name
        goldenFile -- golden file path
        (lazyFastaOut <$> readFastaFile fastaFile) -- action whose result is tested
    | fastaFile <- fastaFiles
    , let goldenFile = replaceExtension fastaFile ".fa-golden"
    ]

-- * unit tests

smallInlineFasta = P.unlines
  [ ">Aaaa"
  , "123"
  , ">Bbbb"
  , "4567"
  , ">Cccc"
  , "890"
  ]

smallTest ∷ Int → Int → Int → Of [(BS.ByteString,BS.ByteString,BS.ByteString)] ()
smallTest h o c = runIdentity
         . toList
         . streamingFasta (HeaderSize h) (OverlapSize o) (CurrentSize c) go
         . S8.fromStrict
         $ BS.pack smallInlineFasta
  where go (Header h) (Overlap o) (Current c _) = yield (h,o,c)

smallTest333 = testCase "3/3/3" $ do
  let res :> r = smallTest 3 3 3
  assertEqual "return is null" () r
  assertEqual "length is 4" 4 (P.length res)
  assertEqual "!!0" (">Aa","","123") (res!!0)
  assertEqual "!!1" (">Bb","","456") (res!!1)
  assertEqual "!!2" (">Bb","456","7") (res!!2)
  assertEqual "!!3" (">Cc","","890") (res!!3)

main :: IO ()
main = do
  gs ← goldenTests
  defaultMain $ testGroup "all tests"
    [ testGroup "Golden" [gs]
    , testGroup "unit tests" [smallTest333]
    ]

-}

