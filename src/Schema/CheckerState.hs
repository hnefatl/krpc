{-# LANGUAGE TemplateHaskell #-}

module Schema.CheckerState where

import Control.Lens (makeLenses)
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Schema.Grammar

-- Template haskell breaks file ordering
-- (https://stackoverflow.com/questions/20876147), so split lensed code into a
-- separate file to avoid confusion.

data FieldStatementState = FieldStatementState
  { _fieldnums :: M.Map FieldNum FieldIdentifier,
    _names :: S.Set FieldIdentifier
  }

instance Default FieldStatementState where
  def = FieldStatementState {_fieldnums = mempty, _names = mempty}

makeLenses ''FieldStatementState