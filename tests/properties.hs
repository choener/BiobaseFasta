
module Main where

import           Control.Lens (view)
import           Control.Monad.Trans.Resource (runResourceT, ResourceT(..), MonadResource)
import           Data.ByteString.Streaming as BSS
import           Data.ByteString.Streaming.Char8 as S8
import           Data.ByteString.Streaming.Internal (ByteString(..))
import           Data.Functor.Of
import           Data.Void
import           Prelude as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Tasty.Golden as Golden
import           Streaming as S
import           Streaming.Prelude as SP
import           System.FilePath (takeBaseName, replaceExtension)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.TH

import           Biobase.Types.BioSequence
import           Biobase.Types.Location
import           Biobase.Types.Strand

import           Biobase.Fasta.Streaming
import           Biobase.Fasta.Strict



-- -- * golden tests
-- 
-- eachFasta (HeaderSize h) (OverlapSize o) (CurrentSize c p) = SP.yield (h,o,c)
-- 
-- readFastaFile ∷ FilePath → IO [(BS.ByteString,BS.ByteString,BS.ByteString)]
-- readFastaFile f = do
--   let s = 1000000
--   xs :> r ← runResourceT
--           $ SP.toList
--           $ streamingFasta (HeaderSize s) (OverlapSize 0) (CurrentSize s) eachFasta
--           $ S8.readFile f
--   return xs
-- 
-- lazyFastaOut = BL8.concat . P.map go
--   where go (h,o,c) = BL8.concat
--           [ BL8.fromStrict h
--           , BL8.pack "\n"
--           , BL8.fromStrict o
--           , BL8.pack "\n"
--           , BL8.fromStrict c
--           , BL8.pack "\n"
--           ]
-- 
-- goldenTests ∷ IO TestTree
-- goldenTests = do
--   fastaFiles ← Golden.findByExtension [".fa"] "./tests/"
--   return $ testGroup "readFastaFile golden tests"
--     [ Golden.goldenVsString
--         (takeBaseName fastaFile) -- test name
--         goldenFile -- golden file path
--         (lazyFastaOut <$> readFastaFile fastaFile) -- action whose result is tested
--     | fastaFile <- fastaFiles
--     , let goldenFile = replaceExtension fastaFile ".fa-golden"
--     ]

-- * unit tests

smallInlineFasta = P.unlines
  [ ">Aaaa"
  , "123"
  , ">Bbbb"
  , "4567"
  , ">Cccc"
  , "890"
  ]

--smallTest ∷ Int → Int → Int → Of [BioSequenceWindow Void Void PartialLocation] ()
--smallTest h o c = runIdentity
--         . toList
----         . SP.map (view windowedFasta)
--         . streamingFasta (HeaderSize h) (OverlapSize o) (CurrentSize c)
--         . S8.fromStrict
--         $ BS.pack smallInlineFasta
--  where go (HeaderSize h) (OverlapSize o) (CurrentSize c) = yield (h,o,c)
--
--smallTest333 = testCase "3/3/3" $ do
--  let res :> r = smallTest 3 3 3
--  assertEqual "return is null" () r
--  assertEqual "length is 4" 4 (P.length res)
--  assertEqual "!!0" (BioSequenceWindow "Aaa" ""    "123" "" (PartialLocation PlusStrand 0 3)) (res!!0)
--  assertEqual "!!1" (BioSequenceWindow "Bbb" ""    "456" "" (PartialLocation PlusStrand 0 3)) (res!!1)
--  assertEqual "!!2" (BioSequenceWindow "Bbb" "456" "7"   "" (PartialLocation PlusStrand 3 1)) (res!!2)
--  assertEqual "!!3" (BioSequenceWindow "Ccc" ""    "890" "" (PartialLocation PlusStrand 0 3)) (res!!3)
--  --
--  assertEqual "!!0/Fasta" (Fasta "Aaa" "123") (view windowedFasta $ res!!0)
--  assertEqual "!!1/Fasta" (Fasta "Bbb" "456") (view windowedFasta $ res!!1)
--  assertEqual "!!2/Fasta" (Fasta "Bbb" "7"  ) (view windowedFasta $ res!!2)
--  assertEqual "!!3/Fasta" (Fasta "Ccc" "890") (view windowedFasta $ res!!3)

main :: IO ()
main = do
--   gs ← goldenTests
   defaultMain $ testGroup "all tests"
--     [ testGroup "Golden" [gs]
--     [ testGroup "unit tests" [smallTest333]
     [
     ]

