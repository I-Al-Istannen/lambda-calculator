module Lambda.Term where
  --( Term(..)
  --, reduce
  --)

data Term = Apply Term Term | Lambda Term | Var Int
  deriving (Show)

repeatUntilNothing :: (a -> Maybe a) -> a -> [a]
repeatUntilNothing f a = a : maybe [] (repeatUntilNothing f) (f a)

reduce :: Term -> Maybe Term
reduce (Apply appl@(Apply _ _) replacement) = Apply <$> reduce appl <*> pure replacement
reduce (Apply (Lambda t) replacement) = Just $ substitute 0 t replacement
reduce _                              = Nothing

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

-- λa b.
termAnd :: Term
termAnd = Lambda (Lambda (Var 0))
