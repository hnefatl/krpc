{-# LANGUAGE OverloadedStrings #-}

module Backends.PythonSpec (spec) where

import Backends.Python
import Control.Monad.Except (runExcept)
import qualified Data.Text as T
import Schema.Grammar
import Schema.Tokeniser
import Test.Hspec

newtype PText = PText T.Text
  deriving (Eq)

instance Show PText where
  show (PText t) = T.unpack t

shouldTranspileTo :: T.Text -> T.Text -> IO ()
shouldTranspileTo input expected = case runExcept (parseGrammar $ tokenise input) of
  Left report -> expectationFailure $ T.unpack $ showReport report
  Right schema -> PText (runLineWriter $ fromSchemaWithoutHeader schema) `shouldBe` PText expected

shouldTranspileTo' :: [T.Text] -> [T.Text] -> IO ()
shouldTranspileTo' x y = shouldTranspileTo (T.unlines x) (T.unlines y)

spec :: Spec
spec = describe "Python Backend" $ do
  it "person demo" $
    shouldTranspileTo'
      [ "message Person {",
        "  optional<string> name = 1;",
        "  optional<int32> age = 2;",
        "  list<bool> bits = 3;",
        "}"
      ]
      [ "import dataclasses",
        "import typing",
        "",
        "@dataclasses.dataclass",
        "class Person:",
        "  name: typing.Optional[str]",
        "  age: typing.Optional[int]",
        "  bits: typing.List[bool]"
      ]