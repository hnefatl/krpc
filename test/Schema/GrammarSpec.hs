{-# LANGUAGE OverloadedStrings #-}

module Schema.GrammarSpec (spec) where

import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import qualified Data.Text as T
import Schema.Grammar
import Schema.Tokeniser
import Test.Hspec

shouldParseTo :: (Parseable a, Eq a, Show a) => T.Text -> a -> IO ()
shouldParseTo input expected =
  (runExcept $ parseGrammar $ tokenise input) `shouldBe` (Right expected)

--shouldFailParse :: (Parseable a, Eq a) => T.Text -> IO ()
--shouldFailParse input expected = shouldThrow anyException $ shouldParseTo input expected

shouldAllParse :: (Parseable a, Eq a, Show a) => [(T.Text, Maybe a)] -> SpecWith ()
shouldAllParse cases = for_ cases $ \(p, expected) ->
  it (unwords [T.unpack p, "should parse to", show expected]) $
    case (runExcept $ parseGrammar $ tokenise p, expected) of
      (Left _, Nothing) -> return ()
      (Right _, Nothing) -> expectationFailure "expected failure but parsed"
      (Left report, Just _) -> expectationFailure $ T.unpack $ showReport report
      (Right actual, Just expected') -> actual `shouldBe` expected'

spec :: Spec
spec = describe "Grammar" $ do
  describe "FieldIdentifier" $
    shouldAllParse
      [ ("name", Just $ FieldIdentifier "name"),
        ("Name", Nothing),
        ("1name", Nothing)
      ]
  describe "TopLevelIdentifier" $
    shouldAllParse
      [ ("Name", Just $ TopLevelIdentifier "Name"),
        ("name", Nothing),
        ("1Name", Nothing)
      ]
  describe "FieldId" $
    shouldAllParse
      [ ("5", Just $ FieldNum 5),
        ("-1", Nothing),
        ("4294967296", Nothing)
      ]
  describe "TypeExpr" $
    shouldAllParse
      [ ("string", Just StringType),
        ("int32", Just Int32Type),
        ("bool", Just BoolType),
        ("optional<int32>", Just $ OptionalType Int32Type),
        ("list<int32>", Just $ ListType Int32Type),
        ("optional<list<int32>>", Just $ OptionalType $ ListType Int32Type),
        ("list< list<bool> >", Just $ ListType $ ListType BoolType)
      ]
  describe "FieldStatement" $
    shouldAllParse
      [ ("string name = 1;", Just $ FieldStatement StringType (FieldIdentifier "name") (FieldNum 1)),
        ("int32 age = 2;", Just $ FieldStatement Int32Type (FieldIdentifier "age") (FieldNum 2)),
        ("list<bool> bits = 3;", Just $ FieldStatement (ListType BoolType) (FieldIdentifier "bits") (FieldNum 3))
      ]
  describe "TopLevelStatement" $
    shouldAllParse
      [ ("message Person { string name = 1; }", Just $ MessageStatement (TopLevelIdentifier "Person") [FieldStatement StringType (FieldIdentifier "name") (FieldNum 1)]),
        ( T.unlines
            [ "message Person {",
              "  string name = 1;",
              "  int32 age = 2;",
              "}"
            ],
          Just $
            MessageStatement
              (TopLevelIdentifier "Person")
              [ FieldStatement StringType (FieldIdentifier "name") (FieldNum 1),
                FieldStatement Int32Type (FieldIdentifier "age") (FieldNum 2)
              ]
        )
      ]