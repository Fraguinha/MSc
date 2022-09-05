module Interpreter where

import Ast (Aexp (..), Bexp (..), Stmt (..))
import Data.Bifunctor as B (Bifunctor (first))
import Data.Map as M (Map, insert, lookup)
import System.Random (Random (randomR), StdGen)

type State = (M.Map String Int, StdGen)

evalAexp :: Aexp -> State -> Maybe Int
evalAexp (AVar str) s = M.lookup str (fst s)
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
evalStmt SSkip s = Just s
evalStmt (SAssign (ident, val)) s = do
  a <- evalAexp val s
  return (B.first (M.insert ident a) s)
evalStmt (SIf (b, s1, s2)) s = do
  b' <- evalBexp b s
  if b' then evalStmt s1 s else evalStmt s2 s
evalStmt (SWhile (b, s1)) s = do
  b' <- evalBexp b s
  if b'
    then do
      s' <- evalStmt s1 s
      evalStmt (SWhile (b, s1)) s'
    else Just s
evalStmt (SSeq (s1, s2)) s = do
  s' <- evalStmt s1 s
  evalStmt s2 s'
evalStmt (SNd (s1, s2)) s =
  let (b, g) = randomR (True, False) (snd s)
      s' = (fst s, g)
   in if b then evalStmt s1 s' else evalStmt s2 s'
