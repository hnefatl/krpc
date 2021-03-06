{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Schema.Grammar where

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad.Except (MonadError, throwError)
import Data.Char (isLower, isUpper)
import qualified Data.Text as T
import Data.Text.Read
import Data.Word (Word32)
import qualified Text.Earley as E
import TextShow

-- Example syntax:
--
-- message Foo {
--   string name = 1;
--   int32 age = 2;
--   list<bool> bits = 3;
-- }

newtype FieldIdentifier = FieldIdentifier T.Text
  deriving (Eq, Ord)

instance Show FieldIdentifier where
  show (FieldIdentifier t) = T.unpack t

instance TextShow FieldIdentifier where
  showb (FieldIdentifier t) = fromText t

newtype TopLevelIdentifier = TopLevelIdentifier T.Text
  deriving (Eq, Ord)

instance Show TopLevelIdentifier where
  show (TopLevelIdentifier t) = T.unpack t

instance TextShow TopLevelIdentifier where
  showb (TopLevelIdentifier t) = fromText t

newtype FieldNum = FieldNum Word32
  deriving (Eq, Ord)

instance Show FieldNum where
  show (FieldNum i) = show i

instance TextShow FieldNum where
  showb (FieldNum i) = fromString (show i)

data TypeExpr
  = StringType
  | Int32Type
  | BoolType
  | OptionalType TypeExpr
  | ListType TypeExpr
  deriving (Eq, Show)

data FieldStatement
  = FieldStatement TypeExpr FieldIdentifier FieldNum
  deriving (Eq, Show)

data TopLevelStatement
  = MessageStatement TopLevelIdentifier [FieldStatement]
  deriving (Eq, Show)

newtype Schema = Schema [TopLevelStatement]

type Prod r a = E.Prod r T.Text T.Text a

type Grammar r a = E.Grammar r (Prod r a)

class Parseable a where
  parse :: forall r. Grammar r a

class SubParseable a where
  subparse :: forall r. Prod r a

instance SubParseable FieldIdentifier where
  subparse = FieldIdentifier <$> E.satisfy (maybe False (isLower . fst) . T.uncons) E.<?> "FieldIdentifier"

instance Parseable FieldIdentifier where
  parse = E.rule subparse

instance SubParseable TopLevelIdentifier where
  subparse = TopLevelIdentifier <$> E.satisfy (maybe False (isUpper . fst) . T.uncons) E.<?> "TopLevelIdentifier"

instance Parseable TopLevelIdentifier where
  parse = E.rule subparse

instance SubParseable FieldNum where
  subparse = FieldNum <$> E.terminal readWord32 E.<?> "FieldNum"
    where
      readWord32 s = case decimal s of
        Left _ -> Nothing
        Right (x, _) -> if x < 0 || x > 4294967295 then Nothing else Just (fromInteger x)

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
    x4 <- E.rule $ OptionalType <$ E.namedToken "optional" <* E.namedToken "<" <*> x1 <* E.namedToken ">" <|> x5
    x5 <- E.rule $ ListType <$ E.namedToken "list" <* E.namedToken "<" <*> x1 <* E.namedToken ">"
    return x1

instance Parseable FieldStatement where
  parse = mdo
    typeExpr <- parse
    E.rule $ FieldStatement <$> typeExpr <*> subparse <* E.namedToken "=" <*> subparse <* E.namedToken ";"

instance Parseable TopLevelStatement where
  parse = mdo
    fieldStatement <- parse
    E.rule $ MessageStatement <$ E.namedToken "message" <*> subparse <* E.namedToken "{" <*> some fieldStatement <* E.namedToken "}"

instance Parseable Schema where
  parse = mdo
    topLevelStatement <- parse
    E.rule $ Schema <$> many topLevelStatement

type Report = E.Report T.Text [T.Text]

parseGrammar :: (Parseable a, MonadError Report m) => [T.Text] -> m a
parseGrammar tokens = case E.fullParses (E.parser parse) tokens of
  ([parsed], _) -> return parsed
  (_, report) -> throwError report

showReport :: Report -> T.Text
showReport (E.Report position expected unconsumed) =
  T.unlines
    [ "error at position " `T.append` showt position,
      "remaining text: " `T.append` showt unconsumed,
      "expected one of: " `T.append` showt expected
    ]