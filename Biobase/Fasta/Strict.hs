
-- | A convenience module for *small* @Fasta@ entries, that are completely in
-- memory and *not* to be streamed.
--
-- The @Data.ByteString.Strict.Lens@ module is very helpful for further
-- handling of 'Fasta' entries.
--
-- For convenience, the 'convertString' function from @string-conversions@ is
-- supplied.

module Biobase.Fasta.Strict
  ( module Biobase.Fasta.Strict
  , convertString
  ) where

import           Control.Lens
import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import           Data.String.Conversions
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS

import           Biobase.Types.BioSequence



-- | A *strict* @Fasta@ entry.

data Fasta which ty = Fasta
  { _header ∷ !(SequenceIdentifier which)
  , _fasta  ∷ !(BioSequence ty)
  }
  deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''Fasta

-- | Render a 'Fasta' entry to a 'ByteString'

fastaToByteString ∷ Int → Fasta which ty → ByteString
{-# Inlinable fastaToByteString #-}
fastaToByteString k' Fasta{..} = BS.cons '>' (_header^._Wrapped) <> go (_fasta^._Wrapped)
  where go (BS.splitAt k → (hd,tl))
          | BS.null hd = mempty
          | otherwise  = hd <> go tl
        k = max 1 k'

-- | Try to parse a 'ByteString' as a 'Fasta', failing with 'Left', succees
-- with 'Right'.

byteStringToFasta ∷ ByteString → Either String (Fasta which ty)
{-# Inlinable byteStringToFasta #-}
byteStringToFasta (BS.lines → ls)
  | null ls = Left "empty bytestring"
  | Just (z, hdr) ← BS.uncons h, z `elem` ">;" = Right $ Fasta { _header = SequenceIdentifier hdr, _fasta = BioSequence $ BS.concat ts }
  | otherwise = Left "no '>'/';' first character"
  where h:ts = ls

-- | A lens that goes from a 'BioSequenceWindow' to a 'Fasta'.

windowedFasta ∷ Lens' (BioSequenceWindow w ty k) (Fasta w ty)
{-# Inline windowedFasta #-}
windowedFasta = lens lr rl
  where lr bsw = Fasta { _header = bsw^.bswIdentifier, _fasta = bsw^.bswSequence }
        rl bsw f = set bswSequence (f^.fasta) $ set bswIdentifier (f^.header) bsw

-- | A prism from a 'ByteString' to a 'Fasta'. Note that this will only be an
-- identity if the underlying fasta file is rendered with @60@ characters per
-- line.

rawFasta ∷ Prism' ByteString (Fasta which ty)
{-# Inline rawFasta #-}
rawFasta = prism (fastaToByteString 60) $ \bs → first (const bs) $ byteStringToFasta bs

