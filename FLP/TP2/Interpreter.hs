module Interpreter where

import Ast (Aexp (..), Bexp (..), Stmt (..))
import Data.Map as M (Map, insert, lookup)

type State = (M.Map String Int)

evalAexp :: Aexp -> State -> Maybe Int
evalAexp (AVar str) s = M.lookup str s
evalAexp (AInt n) s = Just n
evalAexp (AAdd (i1, i2)) s = (+) <$> evalAexp i1 s <*> evalAexp i2 s
evalAexp (ASub (i1, i2)) s = (-) <$> evalAexp i1 s <*> evalAexp i2 s
evalAexp (AMul (i1, i2)) s = (*) <$> evalAexp i1 s <*> evalAexp i2 s

evalBexp :: Bexp -> State -> Maybe Bool
evalBexp BTrue s = Just True
evalBexp BFalse s = Just False
evalBexp (BNot b) s = not <$> evalBexp b s
evalBexp (BAnd (b1, b2)) s = (&&) <$> evalBexp b1 s <*> evalBexp b2 s
evalBexp (BOr (b1, b2)) s = (||) <$> evalBexp b1 s <*> evalBexp b2 s
evalBexp (BLe (i1, i2)) s = (<=) <$> evalAexp i1 s <*> evalAexp i2 s
evalBexp (BEq (i1, i2)) s = (==) <$> evalAexp i1 s <*> evalAexp i2 s

evalStmt :: Stmt -> State -> Maybe State
evalStmt SSkip = Just
evalStmt (SAssign (ident, e)) =
  \s -> do
    a <- evalAexp e s
    return (M.insert ident a s)
evalStmt (SIf (b, s1, s2)) =
  \s -> do
    b <- evalBexp b s
    s1 <- evalStmt s1 s
    s2 <- evalStmt s2 s
    return (if b then s1 else s2)
evalStmt (SWhile (b, s1)) =
  \s -> do
    b' <- evalBexp b s
    if b'
      then do
        s <- evalStmt s1 s
        evalStmt (SWhile (b, s1)) s
      else return s
evalStmt (SSeq (s1, s2)) =
  \s -> do
    s1 <- evalStmt s1 s
    evalStmt s2 s1
