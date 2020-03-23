module Lambda.MegaParsing where

import           Control.Monad
import           Data.Char
import           Data.Void
import           Lambda.Term
import           Text.Megaparsec
import           Text.Megaparsec.Char

data NamedTerm a
 = NApply (NamedTerm a) (NamedTerm a)
 | NLambda a (NamedTerm a)
 | NVar a
 | KnownTerm Term
  deriving Show

type Parser a = Parsec Void String a

lambdaVariable :: Parser String
lambdaVariable = takeWhile1P Nothing isLower

lambdaKnownTerm :: Parser (NamedTerm String)
lambdaKnownTerm
  = foldr1 (<|>) [
    andParser, trueParser, falseParser, orParser, notParser, yParser, sParser, iParser
  ]
  where
    andParser = KnownTerm TermAnd <$ string "AND"
    falseParser = KnownTerm TermF <$ string "F"
    iParser = KnownTerm TermI <$ string "I"
    notParser = KnownTerm TermNot <$ string "NOT"
    orParser = KnownTerm TermOr <$ string "OR"
    sParser = KnownTerm TermS <$ string "S"
    trueParser = KnownTerm TermT <$ string "T"
    yParser = KnownTerm TermY <$ string "Y"

lambdaLambda :: Parser (NamedTerm String)
lambdaLambda = do
  void $ single '\\'
  variables <- sepBy1 lambdaVariable space
  void $ single '.' <* optional space
  expression <- lambdaTerm
  pure $ foldr NLambda (NLambda (last variables) expression) (init variables)

lambdaApply :: Parser (NamedTerm String)
lambdaApply = foldl1 NApply <$> sepBy1 partialExpression space
  where
    inParens = between (single '(') (single ')') lambdaTerm
    partialExpression = inParens <|> NVar <$> lambdaVariable <|> lambdaKnownTerm

lambdaTerm :: Parser (NamedTerm String)
lambdaTerm = (try lambdaApply <|> try lambdaKnownTerm <|> lambdaLambda <|> NVar <$> lambdaVariable) <* space

completeExpression :: Parser (NamedTerm String)
completeExpression = do
  term <- lambdaTerm
  void eof
  return term

displayParse :: (Show a) => Parser a -> String -> IO ()
displayParse parser input = do
  let res = parse parser "" input
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right r  -> print r

parseOrError :: String -> NamedTerm String
parseOrError input = do
  let res = parseInput input
  case res of
    Left err -> error $ errorBundlePretty err
    Right r  -> r

parseInput :: String -> Either (ParseErrorBundle String Void) (NamedTerm String)
parseInput = parse completeExpression ""
