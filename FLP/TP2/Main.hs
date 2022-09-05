module Main where

import Data.Foldable as F (mapM_)
import Data.Map as M (empty, toList)
import Interpreter (evalStmt)
import Lexer (alexScanTokens)
import Parser (parse)

main :: IO ()
main = do
  s <- getContents
  let ast = parse (alexScanTokens s)
      l = fmap M.toList (evalStmt ast M.empty)
   in F.mapM_ print l
