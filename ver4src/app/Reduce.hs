{-
◆author: righ1113
◆動かし方
1. $ stack run reduce-exe
1. $ stack ghci --main-is ver4src:exe:reduce-exe
2. > main
2. > :l app/Reduce
-}
module Main where

import CoLibCConst ( readFileGoodConfsR )


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"
  gConfs <- readFileGoodConfsR
  print (((gConfs !! 1) !! 2) !! 1)
  -- mainLoop graphs
  -- putStrLn "633個の好配置は全て、Ｄ可約 or Ｃ可約です"
  putStrLn "プログラムは正常終了しました"



