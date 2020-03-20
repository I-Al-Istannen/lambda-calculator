{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lambda.Term where
  --( Term(..)
  --, reduce
  --)

import           Control.Applicative
import           Data.Char
import           Data.Foldable

data Term = Apply Term Term | Lambda Term | Var Int
  deriving (Show)

mapVars :: (Int -> Int) -> Term -> Term
mapVars f (Apply a b) = Apply (mapVars f a) (mapVars f b)
mapVars f (Lambda t)  = Lambda (mapVars f t)
mapVars f (Var n)     = Var (f n)

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
substitute depth (Lambda t) replacement =
  Lambda $ substitute (depth + 1) t $ incrementFreeVars 0 replacement

incrementFreeVars :: Int -> Term -> Term
incrementFreeVars depth (Var n)
  | n >= depth = Var (n + 1)
  | otherwise  = Var n
incrementFreeVars depth (Apply a b) = Apply (incrementFreeVars depth a) (incrementFreeVars depth b)
incrementFreeVars depth (Lambda t) = Lambda $ incrementFreeVars (depth + 1) t

showTerm :: Int -> Term -> String
showTerm _ (showFancy -> Just x)   = x
showTerm n (Var value)             = showVar (n - 1 - value)
showTerm n (Lambda inner)          = '\\' : showVar n ++ showInnerLambda (n + 1) inner
showTerm n (Apply l@(Apply _ _) r) = showTerm n l ++ " " ++ showTermMightParen n r
showTerm n (Apply l r)             = showTermMightParen n l ++ " " ++ showTermMightParen n r

showTermMightParen :: Int -> Term -> String
showTermMightParen n a@(Var _)               = showTerm n a
showTermMightParen n a@(showFancy -> Just _) = showTerm n a
showTermMightParen n a                       = "(" ++ showTerm n a ++ ")"

showVar :: Int -> String
showVar n
  | n < 26    = [chr (n + ord 'a')]
  | otherwise = "<<" ++ show n ++ ">>"

showInnerLambda :: Int -> Term -> String
showInnerLambda n (Lambda inner) = " " ++ showVar n ++ showInnerLambda (n + 1) inner
showInnerLambda n t              = "." ++ showTerm n t

-- Fancy function naming

showFancy :: Term -> Maybe String
showFancy TermI   = Just "I"
--showFancy TermK   = Just "K" -- Uncomment for the "proper" name :P
showFancy TermS   = Just "S"
showFancy TermY   = Just "Y"
showFancy TermT   = Just "T"
showFancy TermF   = Just "F"
showFancy TermNot = Just "NOT"
showFancy TermAnd = Just "AND"
showFancy TermOr  = Just "OR"
showFancy _       = Nothing

-- I: \a.a
pattern TermI :: Term
pattern TermI = Lambda (Var 0)

-- K: \a b.a
pattern TermK :: Term
pattern TermK = Lambda2 (Var 1)

-- S: \a b c.a c (b c)
pattern TermS :: Term
pattern TermS = Lambda3 (Apply3 (Var 2) (Var 0) (Apply (Var 1) (Var 0)))

-- Y: \f.(\x.f (x x)) (\x.f (x x))
pattern TermY :: Term
pattern TermY = Lambda (Apply
                         (Lambda (Apply (Var 1) (Apply (Var 0) (Var 0))))
                         (Lambda (Apply (Var 1) (Apply (Var 0) (Var 0))))
                       )

-- Boolean expressions

-- T: \a b.a
pattern TermT :: Term
pattern TermT = Lambda2 (Var 1)

-- F: \a b.b
pattern TermF :: Term
pattern TermF = Lambda2 (Var 0)

-- NOT: \a.a F T
pattern TermNot :: Term
pattern TermNot = Lambda (Apply3 (Var 0) TermF TermT)

-- AND: \a b.a b F
pattern TermAnd :: Term
pattern TermAnd = Lambda2 (Apply3 (Var 1) (Var 0) TermF)

-- OR: \a b.a T b
pattern TermOr :: Term
pattern TermOr = Lambda2 (Apply3 (Var 1) TermT (Var 0))

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
