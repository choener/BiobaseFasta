{-# Language DeriveGeneric #-}

module Biobase.Fasta.Types where

import Control.DeepSeq
import Control.Lens
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data
import GHC.Generics
import Biobase.Types.NucleotideSequence
import Biobase.Types.AminoAcidSequence

-- |

data Fasta = Fasta { fastaHeader :: B.ByteString, fastaSequence :: B.ByteString }
  deriving (Eq)

newtype RawFastaEntry = RawFastaEntry { _rawFastaEntry :: ByteString }
  deriving (Show,Eq,Ord,Typeable)

--makeLenses ''RawFastaEntry

-- | 'StreamEvent's are chunked pieces of data, where the raw data is
-- a strict @ByteString@. Each element also retains information on the
-- first and last line and column (via 'streamLines') that are part of this
-- chunk.

data StreamEvent
  -- | A Header event, multiple header events signal that the header name
  -- was longer than the chunk size.
  = StreamHeader  { streamHeader  :: !ByteString, streamLines :: !LineInfo }
  -- | A data event. We keep a pointer to the previous chunk (which is
  -- useful for some algorithms). The chunk is free of newlines!
  | StreamFasta   { streamFasta   :: !ByteString, prevStreamFasta :: !ByteString, streamLines :: !LineInfo, streamHeader :: !ByteString }
  deriving (Show,Eq,Ord,Typeable,Generic)

instance NFData StreamEvent



-- | Complete information on line and column start and end for a chunk.
--
-- TODO This is a 1-based format? Lets use the BiobaseTypes facilities!

data LineInfo = LineInfo
  { firstLine   :: !Int   -- ^ first line for this chunk @(lines in complete file!)@
  , firstCol    :: !Int   -- ^ first column in first line for this chunk
  , lastLine    :: !Int   -- ^ last line for this chunk @(lines in complete file!)@
  , lastCol     :: !Int   -- ^ last column in last line for this chunk
  , firstIndex  :: !Int   -- ^ first index in this fasta block. Counts just the number of symbols in the @Fasta@ payload.
  }
  deriving (Show,Eq,Ord,Typeable,Generic)

instance NFData LineInfo

