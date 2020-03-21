module Lambda.Parsing where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Char
import           Data.List

newtype Parser a = Parser (String -> Either [String] (a, String))

parse :: Parser a -> String -> Either [String] (a, String)
parse (Parser f) = f

instance Functor Parser where
  fmap f parser = Parser (fmap (first f) . parse parser)

instance Applicative Parser where
  pure x = Parser (\input -> Right (x, input))

  boxedF <*> boxedVal = Parser (\input -> case parse boxedF input of
    Right (f, rest) -> parse (fmap f boxedVal) rest
    Left msg        -> Left msg
    )

instance Monad Parser where
  boxedVal >>= f = Parser (\input -> case parse boxedVal input of
    Right (val, rest) -> parse (f val) rest
    Left msg          -> Left msg
    )

instance Alternative Parser where
  empty = Parser (const $ Left [])

  a <|> b = Parser (\input -> case parse a input of
    Right (val, rest) -> Right (val, rest)
    Left aErr            -> case parse b input of
      Left bErr -> Left $ aErr <> bErr
      right     -> right)

errorMessage :: String -> Parser a
errorMessage msg = Parser $ const $ Left [msg]

label :: String -> Parser a -> Parser a
label msg parser = Parser (\input -> case parse parser input of
  Left _ -> Left [msg]
  x      -> x)

(<?>) ::  Parser a -> String -> Parser a
parser <?> msg = label msg parser

-- Simple parsers
anySingle :: Parser Char
anySingle = Parser (\input -> case input of
  (x:xs) -> Right (x, xs)
  _      -> Left ["a character"])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  single <- anySingle
  if p single then pure single
  else errorMessage (show single ++ " to satisfy a predicate")

char :: Char -> Parser Char
char c = satisfy (== c) <?> show c

letterChar :: Parser Char
letterChar = satisfy isLetter

prettyParse :: Parser a -> String -> Either String (a, String)
prettyParse parser input = case parse parser input of
  Left alternatives -> Left $ "Expected " ++ foldr (<>) "" (intersperse " or " alternatives)
  Right (a, rest)   -> Right (a, rest)
