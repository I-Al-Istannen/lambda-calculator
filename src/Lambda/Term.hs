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
