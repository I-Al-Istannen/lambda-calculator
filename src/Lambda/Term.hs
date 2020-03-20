{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lambda.Term where
  --( Term(..)
  --, reduce
  --)

import           Control.Applicative
import           Data.Char

data Term = Apply Term Term | Lambda Term | Var Int
  deriving (Show)

repeatUntilNothing :: (a -> Maybe a) -> a -> [a]
repeatUntilNothing f a = a : maybe [] (repeatUntilNothing f) (f a)

reduce :: Term -> Maybe Term
reduce (Apply (Lambda t) b) = Just $ substitute 0 t b
reduce (Apply a b)          = (Apply <$> reduce a <*> pure b) <|> (Apply a <$> reduce b)
reduce (Lambda t)           = Lambda <$> reduce t
reduce _                    = Nothing

substitute :: Int -> Term -> Term -> Term
substitute depth (Var n) replacement
  | n > depth  = Var (n - 1)
  | n == depth = replacement
  | otherwise  = Var n
substitute depth (Apply l r) replacement =
  Apply (substitute depth l replacement) (substitute depth r replacement)
substitute depth (Lambda t) replacement = Lambda $ substitute (depth + 1) t replacement

-- λa b.a
termT :: Term
termT = Lambda (Lambda (Var 1))

-- λa b.b
termF :: Term
termF = Lambda (Lambda (Var 0))

-- λa. (λc d.c)
-- (λa b.a) (λc d.d)
-- (λb.(λc d.d))

-- (λa b.b) (λc d.d)
-- (λb.b)
-- λa.a termF termT
-- \a F T
termNot :: Term
termNot = Lambda (Apply (Apply (Var 0) termF) termT)

-- λa b.a b F
termAnd :: Term
termAnd = Lambda $ Lambda (Apply (Apply (Var 1) (Var 0)) termF)

showTerm :: Int -> Term -> String
showTerm _ (showFancy -> Just x)   = x
showTerm n (Var value)             = showVar (n - 1 - value)
showTerm n (Lambda inner)          = '\\' : showVar n ++ showInnerLambda (n + 1) inner
showTerm n (Apply l@(Apply _ _) r) = showTerm n l ++ " " ++ showTermMightParen n r
showTerm n (Apply l r)             = showTermMightParen n l ++ " " ++ showTermMightParen n r

showTermMightParen :: Int -> Term -> String
showTermMightParen n a@(Var _) = showTerm n a
showTermMightParen n a         = "(" ++ showTerm n a ++ ")"

showVar :: Int -> String
showVar n
  | n <= 26   = [chr (n + ord 'a')]
  | otherwise = "<<" ++ show n ++ ">>"

showInnerLambda :: Int -> Term -> String
showInnerLambda n (Lambda inner) = " " ++ showVar n ++ showInnerLambda (n + 1) inner
showInnerLambda n t              = "." ++ showTerm n t

-- Fancy function naming

showFancy :: Term -> Maybe String
showFancy TermT   = Just "T"
showFancy TermF   = Just "F"
showFancy TermAnd = Just "AND"
showFancy TermId  = Just "ID"
showFancy _       = Nothing


pattern TermT :: Term
pattern TermT = Lambda2 (Var 1)
pattern TermF :: Term
pattern TermF = Lambda2 (Var 0)
pattern TermAnd :: Term
pattern TermAnd = Lambda2 (Apply3 (Var 1) (Var 0) (Lambda2 (Var 0)))
pattern TermId :: Term
pattern TermId = Lambda (Var 0)

pattern Apply3 :: Term -> Term -> Term -> Term
pattern Apply3 a b c = Apply (Apply a b) c
pattern Apply4 :: Term -> Term -> Term -> Term -> Term
pattern Apply4 a b c d = Apply (Apply3 a b c) d
pattern Apply5 :: Term -> Term -> Term -> Term -> Term -> Term
pattern Apply5 a b c d e = Apply (Apply4 a b c d) e
pattern Apply6 :: Term -> Term -> Term -> Term -> Term -> Term -> Term
pattern Apply6 a b c d e f = Apply (Apply5 a b c d e) f
pattern Apply7 :: Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term
pattern Apply7 a b c d e f g = Apply (Apply6 a b c d e f) g

pattern Lambda2 :: Term -> Term
pattern Lambda2 a = Lambda (Lambda a)
pattern Lambda3 :: Term -> Term
pattern Lambda3 a = Lambda (Lambda2 a)
pattern Lambda4 :: Term -> Term
pattern Lambda4 a = Lambda (Lambda3 a)
pattern Lambda5 :: Term -> Term
pattern Lambda5 a = Lambda (Lambda4 a)
pattern Lambda6 :: Term -> Term
pattern Lambda6 a = Lambda (Lambda5 a)
pattern Lambda7 :: Term -> Term
pattern Lambda7 a = Lambda (Lambda6 a)
