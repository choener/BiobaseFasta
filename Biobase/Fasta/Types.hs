{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | Encoding of Fasta

module Biobase.Fasta.Types where

import Prelude
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf
import Data.Int

data FastaWindow = FastaW
  { _identifier   :: !B.ByteString    -- ^ the current identifier
  , _description  :: !B.ByteString    -- ^ and description, if any
  , _offset       :: !Int64       -- ^ Zero-based offset into the current stream
  , _fasta        :: !B.ByteString    -- ^ window data
  , _past         :: !B.ByteString    -- ^ the last window we saw. "" on first window.
  }
  deriving (Show,Eq)

data Fasta = Fasta
    { header :: !B.ByteString,
      sequence :: !B.ByteString
    }
    deriving (Show, Eq)
