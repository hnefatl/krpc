{-# LANGUAGE OverloadedStrings #-}

module Schema.TokeniserSpec (spec) where

import qualified Data.Text as T
import Schema.Tokeniser
import Test.Hspec

tokenisesTo :: T.Text -> [T.Text] -> SpecWith ()
tokenisesTo input expected = it (T.unpack input) $ tokenise input `shouldBe` expected

spec :: Spec
spec = describe "Tokeniser" $ do
  "" `tokenisesTo` []
  "hi" `tokenisesTo` ["hi"]
  "hi_" `tokenisesTo` ["hi_"]
  "hi-" `tokenisesTo` ["hi", "-"]
  "hi-foo" `tokenisesTo` ["hi", "-", "foo"]
  "hi * 23" `tokenisesTo` ["hi", "*", "23"]
  "hi*23" `tokenisesTo` ["hi", "*", "23"]