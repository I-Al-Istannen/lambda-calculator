{-# LANGUAGE BangPatterns #-}
module Lambda.MegaParsing where

import           Data.Char
import           Data.Void
-- import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char

data NamedTerm a = NApply (NamedTerm a) (NamedTerm a) | NLambda a (NamedTerm a) | NVar a
  deriving Show

type Parser a = Parsec Void String a

lambdaVariable :: Parser String
lambdaVariable = takeWhile1P Nothing isLower

lambdaLambda :: Parser (NamedTerm String)
lambdaLambda = do
  _ <- single '\\'
  initialVar <- lambdaVariable
  followingVars <- reverse <$> many (space *> lambdaVariable)
  let reorderedVars = followingVars ++ [initialVar]
  _ <- single '.'
  end <- lambdaTerm
  let finalLambda = foldr NLambda (NLambda (head reorderedVars) end) (tail reorderedVars)
  pure finalLambda

lambdaApply :: Parser (NamedTerm String)
lambdaApply  = do
  others <- (:) <$> applicationBeginning <*> some applicationPart
  let finalApply = foldl1 NApply others
  return finalApply
  where
    inParens = do
        _ <- single '('
        lhs <- lambdaTerm
        _ <- single ')'
        return lhs
    applicationBeginning = inParens <|> NVar <$> lambdaVariable
    applicationPart = do
      space
      inParens <|> NVar <$> lambdaVariable

lambdaTerm :: Parser (NamedTerm String)
lambdaTerm = try lambdaApply <|> lambdaLambda <|> NVar <$> lambdaVariable

displayParse :: (Show a) => Parser a -> String -> IO ()
displayParse parser input = do
  let res = parse parser "" input
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right r  -> print r
