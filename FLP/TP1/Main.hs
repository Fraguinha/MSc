module Main where

import Data.Foldable as F (mapM_)
import Data.Map as M (empty, toList)
import Interpreter (evalStmt)
import Lexer (alexScanTokens)
import Parser (parse)
import System.Random as R (newStdGen)

main :: IO ()
main = do
  g <- R.newStdGen
  s <- getContents
  let ast = parse (alexScanTokens s)
      l = fmap (M.toList . fst) (evalStmt ast (M.empty, g))
   in F.mapM_ print l
