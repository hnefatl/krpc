{-# LANGUAGE OverloadedStrings #-}

module Schema.Tokeniser (tokenise) where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.Text as T

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- Collect consecutive runs of alphanumeric/spaces, split on anything else
collectTokens :: [T.Text] -> T.Text -> [T.Text]
collectTokens xs "" = xs
collectTokens xs s = case T.span isIdentifierChar s of
  -- If no identifier chars, then try collecting whitespace
  ("", _) -> case T.span isSpace s of
    -- If no whitespace, then just collect the head character
    ("", _) -> collectTokens (T.singleton (T.head s) : xs) (T.tail s)
    -- Skip any whitespace spans
    (_, s') -> collectTokens xs s'
  -- Keep track of identifier spans
  (x, s') -> collectTokens (x : xs) s'

tokenise :: T.Text -> [T.Text]
tokenise = reverse . collectTokens []
