module Lambda.MegaParsing where

import           Control.Monad
import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

data NamedTerm a = NApply (NamedTerm a) (NamedTerm a) | NLambda a (NamedTerm a) | NVar a
  deriving Show

type Parser a = Parsec Void String a

lambdaVariable :: Parser String
lambdaVariable = takeWhile1P Nothing isLower

lambdaLambda :: Parser (NamedTerm String)
lambdaLambda = do
  void $ single '\\'
  variables <- sepBy1 lambdaVariable space
  void $ single '.'
  expression <- lambdaTerm
  pure $ foldr NLambda (NLambda (last variables) expression) (init variables)

lambdaApply :: Parser (NamedTerm String)
lambdaApply = foldl1 NApply <$> sepBy1 partialExpression space
  where
    inParens = between (single '(') (single ')') lambdaTerm
    partialExpression = inParens <|> NVar <$> lambdaVariable

lambdaTerm :: Parser (NamedTerm String)
lambdaTerm = try lambdaApply <|> lambdaLambda <|> NVar <$> lambdaVariable

displayParse :: (Show a) => Parser a -> String -> IO ()
displayParse parser input = do
  let res = parse parser "" input
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right r  -> print r

parseOrError :: String -> NamedTerm String
parseOrError input = do
  let res = parse lambdaTerm "" input
  case res of
    Left err -> error $ errorBundlePretty err
    Right r  -> r
