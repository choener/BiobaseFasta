{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | This module is currently home to a preliminary version of indices based
-- on a minimal index of 'Zero' or 'One' (and possibly others).

module Biobase.Fasta where

import Data.ByteString.Char8 (ByteString)
import Bio.Core.Sequence (Offset(..))



data FastaWindow = FastaW
  { _identifier   :: !ByteString    -- ^ the current identifier
  , _description  :: !ByteString    -- ^ and description, if any
  , _offset       :: !Offset        -- ^ Zero-based offset into the current stream
  , _fasta        :: !ByteString    -- ^ window data
  , _past         :: !ByteString    -- ^ the last window we saw. "" on first window.
  }
  deriving (Show)
