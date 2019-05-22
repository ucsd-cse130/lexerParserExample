module Language.Types where

data Expr
  = EInt Int
  | EPlus Expr Expr
  | EMul Expr Expr
  deriving (Eq, Show)
