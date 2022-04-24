{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Schema.Grammar where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad.Except (MonadError, throwError)
import Data.Char (isLower, isUpper)
import Data.Maybe (listToMaybe)
import Data.Word (Word32)
import qualified Text.Earley as E
import Text.Read (readMaybe)

-- Example syntax:
--
-- message Foo {
--   string name = 1;
--   int32 age = 2;
--   list<bool> bits = 3;
-- }

newtype FieldIdentifier = FieldIdentifier String
  deriving (Eq, Show)

newtype TopLevelIdentifier = TopLevelIdentifier String
  deriving (Eq, Show)

newtype FieldNum = FieldNum Word32
  deriving (Eq, Show)

data TypeExpr
  = StringType
  | Int32Type
  | BoolType
  | ListType TypeExpr
  deriving (Eq, Show)

data FieldStatement
  = FieldStatement TypeExpr FieldIdentifier FieldNum
  deriving (Eq, Show)

data TopLevelStatement
  = MessageStatement TopLevelIdentifier [FieldStatement]
  deriving (Eq, Show)

type Prod r a = E.Prod r String String a

type Grammar r a = E.Grammar r (Prod r a)

class Parseable a where
  parse :: forall r. Grammar r a

class SubParseable a where
  subparse :: forall r. Prod r a

instance SubParseable FieldIdentifier where
  subparse = FieldIdentifier <$> E.satisfy (maybe False isLower . listToMaybe)

instance Parseable FieldIdentifier where
  parse = E.rule subparse

instance SubParseable TopLevelIdentifier where
  subparse = TopLevelIdentifier <$> E.satisfy (maybe False isUpper . listToMaybe)

instance Parseable TopLevelIdentifier where
  parse = E.rule subparse

instance SubParseable FieldNum where
  subparse = FieldNum <$> E.terminal readWord32
    where
      readWord32 s =
        readMaybe s >>= \x ->
          if x < 0 || x > 4294967295 then Nothing else Just (fromInteger x)

instance Parseable FieldNum where
  parse = E.rule subparse

instance Parseable TypeExpr where
  parse = mdo
    -- This could be rewritten in normal applicative form by using `parseTypeExpr`
    -- for the recursive expression below, but this form using `rule` allows for
    -- left-recursion thanks to Earley.
    -- This is somewhat reminiscent of BNF, each x<n> is a rule.
    x1 <- E.rule $ StringType <$ E.namedToken "string" <|> x2
    x2 <- E.rule $ Int32Type <$ E.namedToken "int32" <|> x3
    x3 <- E.rule $ BoolType <$ E.namedToken "bool" <|> x4
    x4 <- E.rule $ ListType <$ E.namedToken "list<" <*> x1 <* E.namedToken ">"
    return x1

instance Parseable FieldStatement where
  parse = mdo
    typeExpr <- parse
    identifier <- parse
    _ <- E.rule $ E.namedToken "="
    fieldNum <- parse
    E.rule $ FieldStatement <$> typeExpr <*> identifier <*> fieldNum

instance Parseable TopLevelStatement where
  parse = mdo
    identifier <- parse
    fieldStatement <- parse
    E.rule $ MessageStatement <$> identifier <*> some fieldStatement

type Report = E.Report String [String]

parseGrammar :: (Parseable a, MonadError Report m) => String -> m a
parseGrammar text = case E.fullParses (E.parser parse) [text] of
  ([parsed], _) -> return parsed
  (_, report) -> throwError report

showReport :: Report -> String
showReport (E.Report position expected unconsumed) =
  unlines
    [ "error at position " ++ show position,
      "remaining text: " ++ show unconsumed,
      "expected one of: " ++ show expected
    ]