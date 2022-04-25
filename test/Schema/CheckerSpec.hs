{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema.CheckerSpec (spec) where

import Control.Monad.Except (Except, MonadError (throwError), catchError, runExcept, unless, withExcept)
import qualified Data.Text as T
import Schema.Checker
import Schema.Grammar
import Schema.Tokeniser
import Test.Hspec

shared :: (Schema -> Except T.Text ()) -> T.Text -> SpecWith ()
shared action input = it (T.unpack input) $
  either (expectationFailure . T.unpack) pure $
    runExcept $ do
      parsedSchema <- withExcept showReport $ parseGrammar $ tokenise input
      action parsedSchema

shouldCheck :: T.Text -> SpecWith ()
shouldCheck = shared checkSchema

shouldNotCheck :: T.Text -> SpecWith ()
shouldNotCheck = shared $ \schema -> do
  failed <- catchError (checkSchema schema >> return False) (\_ -> return True)
  unless failed $ throwError "schema checked but should have failed"

spec :: Spec
spec = describe "Schema Checker" $ do
  shouldCheck "message Foo { string name = 1; int32 age = 2; }"
  shouldNotCheck "message Foo { string name = 1; int32 age = 1; }"
  shouldNotCheck "message Foo { string name = 1; int32 name = 2; }"