module Lambda.Term
  ( Term(..)
  ) where

data Term = Apply Term Term | Lambda String Term | Var String
  deriving (Show)
