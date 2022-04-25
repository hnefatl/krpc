{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Schema.Checker where

import Control.Lens (Lens', use, (%=))
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Schema.CheckerState
import Schema.Grammar
import TextShow (TextShow (showt))

-- Utilities
mapError :: MonadError e m => (e -> e) -> m a -> m a
mapError f = flip catchError (throwError . f)

setLookup :: Ord a => a -> S.Set a -> Maybe ()
setLookup val s = if S.member val s then Just () else Nothing

insertOrFailGeneric :: (MonadError e m, MonadState s m) => Lens' s s' -> (s' -> Maybe a) -> (s' -> s') -> (a -> e) -> m ()
insertOrFailGeneric lens flookup finsert ferr = do
  s <- use lens
  case flookup s of
    Nothing -> lens %= finsert
    Just existing -> throwError (ferr existing)

insertOrFailSet :: (MonadError e m, MonadState s m, Ord a) => Lens' s (S.Set a) -> a -> e -> m ()
insertOrFailSet lens val err = insertOrFailGeneric lens (setLookup val) (S.insert val) (const err)

insertOrFailMap :: (MonadError e m, MonadState s m, Ord k) => Lens' s (M.Map k v) -> k -> v -> (v -> e) -> m ()
insertOrFailMap lens key val = insertOrFailGeneric lens (M.lookup key) (M.insert key val)

-- Actual functions

checkSchema :: MonadError T.Text m => Schema -> m ()
checkSchema (Schema tls) = evalStateT (mapM_ checkTopLevelStatement tls) mempty

checkTopLevelStatement :: (MonadError T.Text m, MonadState (S.Set TopLevelIdentifier) m) => TopLevelStatement -> m ()
checkTopLevelStatement (MessageStatement name fields) = do
  insertOrFailSet id name $ T.unwords ["statement", showt name, "uses same name as existing top-level statement"]
  mapError (\e -> T.unwords ["in message", showt name, e]) $
    evalStateT (mapM_ checkFieldStatement fields) def

checkFieldStatement :: (MonadError T.Text m, MonadState FieldStatementState m) => FieldStatement -> m ()
checkFieldStatement (FieldStatement _ name num) = do
  insertOrFailMap fieldnums num name $ \existingName -> T.unwords ["field", showt name, "uses same id as existing field", showt existingName]
  insertOrFailSet names name $ T.unwords ["field", showt name, "uses same name as existing field"]