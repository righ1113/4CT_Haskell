-- ghc-9.0.2
-- $ cd fourcolor2
-- $ code .
-- $ stack run
module Main ( main ) where

import Reduce    ( reduce    )
import Discharge ( discharge )


main :: IO ()
main = do
  gConfsStrRe <- readFile "data/ReGoodConfs20.txt"

  rulesStr    <- readFile "data/DiRules07.txt"
  gConfsStrDi <- readFile "data/DiGoodConfs.txt"
  tacStr      <- readFile "data/DiTactics07.txt"

  putStrLn $ "reduce    = " ++ show (reduce gConfsStrRe)
  putStrLn $ "discharge = " ++ show (discharge (7, rulesStr, gConfsStrDi, tacStr))



