
module Biobase.Fasta.Types where

import GHC.Generics
import Data.Data
import Data.ByteString.Char8 (ByteString)
import Control.DeepSeq



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
  | StreamFasta   { streamFasta   :: !ByteString, prevStreamFasta :: !ByteString, streamLines :: !LineInfo }
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

instance NFData StreamEvent



-- | Complete information on line and column start and end for a chunk.
--
-- TODO This is a 1-based format? Lets use the BiobaseTypes facilities!

data LineInfo = LineInfo
  { firstLine :: !Int
  , firstCol  :: !Int
  , lastLine  :: !Int
  , lastCol   :: !Int
  }
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

instance NFData LineInfo

