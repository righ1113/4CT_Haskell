{-
◆author: righ1113
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「unavoidable.conf」を取得し、
    ver2src/readFile/に置く。
2. > stack run reduce-exe
-}
module Main where
-- module Reduce where

{-
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Control.Arrow ((<<<))
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, _4, _5, _6, _7, (^.))
import Debug.Trace (trace)
-}
import Lib (myLoop)


verts   = 27 -- max number of vertices in a free completion + 1
deg     = 13 -- max degree of a vertex in a free completion + 1
    -- must be at least 13 because of row 0
edges   = 62 -- max number of edges in a free completion + 1
maxring = 14 -- max ring-size


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"
  putStrLn "プログラムは正常終了しました"



