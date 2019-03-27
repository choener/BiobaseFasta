
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
import           Data.Void
import           GHC.Generics (Generic)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude as SP

import           Biobase.Fasta.Streaming as FS
import           Biobase.Types.BioSequence



-- | A *strict* @Fasta@ entry.

data Fasta which ty = Fasta
  { _header ∷ !(SequenceIdentifier which)
  , _fasta  ∷ !(BioSequence ty)
  }
  deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''Fasta

-- | If you don't want to deal with the phantom types.

type FastaUntyped = Fasta Void Void

-- | Render a 'Fasta' entry to a 'ByteString'. Will end with a final @\n@ in
-- any case.

fastaToByteString ∷ Int → Fasta which ty → ByteString
{-# Inlinable fastaToByteString #-}
fastaToByteString k' Fasta{..} = BS.cons '>' (_header^._Wrapped) <> "\n" <> go (_fasta^._Wrapped)
  where go (BS.splitAt k → (hd,tl))
          | BS.null hd = mempty
          | otherwise  = hd <> "\n" <> go tl
        k = max 1 k'

-- | Render a 'Fasta' entry to a 'Builder'. Will end with a final @\n@ in
-- any case.

fastaToBuilder ∷ Int → Fasta which ty → BB.Builder
{-# Inlinable fastaToBuilder #-}
fastaToBuilder k' Fasta{..} = BB.char8 '>' <> (BB.byteString $ _header^._Wrapped) <> BB.char8 '\n' <> go (_fasta^._Wrapped)
  where go (BS.splitAt k → (hd,tl))
          | BS.null hd = mempty
          | otherwise  = BB.byteString hd <> BB.char8 '\n' <> go tl
        k = max 1 k'

-- | Try to parse a 'ByteString' as a 'Fasta', failing with 'Left', succees
-- with 'Right'.

byteStringToFasta ∷ ByteString → Either String (Fasta which ty)
{-# Inlinable byteStringToFasta #-}
byteStringToFasta (BS.lines → ls)
  | null ls = Left "empty bytestring"
  | Just (z, hdr) ← BS.uncons h, z `BS.elem` ">;" = Right $ Fasta { _header = SequenceIdentifier hdr, _fasta = BioSequence $ BS.concat ts }
  | otherwise = Left "no '>'/';' first character"
  where h:ts = ls

-- | Try to parse a 'ByteString' as multiple 'Fasta' entries. Even though this
-- is using the underlying streaming interface, this is not streaming.

byteStringToMultiFasta
  ∷ BSL.ByteString → [Fasta which ty]
{-# Inlinable byteStringToMultiFasta #-}
byteStringToMultiFasta bsl = map (view windowedFasta) $ runIdentity bss
  where bss = SP.toList_ . streamingFasta (HeaderSize maxBound) (OverlapSize 0) (CurrentSize maxBound) $ BSS.fromLazy bsl

-- | A lens that goes from a 'BioSequenceWindow' to a 'Fasta'.

windowedFasta ∷ Lens' (BioSequenceWindow w ty k) (Fasta w ty)
{-# Inline windowedFasta #-}
windowedFasta = lens lr rl
  where lr bsw = Fasta { _header = bsw^.bswIdentifier, _fasta = bsw^.bswSequence }
        rl bsw f = set bswSequence (f^.fasta) $ set bswIdentifier (f^.header) bsw

-- | A prism from a 'ByteString' to a 'Fasta'. Note that this will only be an
-- identity if the underlying fasta file is rendered with @k@ characters per
-- line.

rawFasta ∷ Int → Prism' ByteString (Fasta which ty)
{-# Inline rawFasta #-}
rawFasta k = prism (fastaToByteString k) $ \bs → first (const bs) $ byteStringToFasta bs

