module Ast where

data Aexp
  = AVar String
  | AInt Int
  | AAdd (Aexp, Aexp)
  | ASub (Aexp, Aexp)
  | AMul (Aexp, Aexp)
  deriving (Show, Eq)

data Bexp
  = BTrue
  | BFalse
  | BNot Bexp
  | BAnd (Bexp, Bexp)
  | BOr (Bexp, Bexp)
  | BLe (Aexp, Aexp)
  | BEq (Aexp, Aexp)
  deriving (Show, Eq)

data Stmt
  = SSkip
  | SAssign (String, Aexp)
  | SIf (Bexp, Stmt, Stmt)
  | SWhile (Bexp, Stmt)
  | SSeq (Stmt, Stmt)
  deriving (Show, Eq)
