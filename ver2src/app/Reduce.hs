{-
◆author: righ1113
◆動かし方
1. > stack run reduce-exe
1. > stack ghci --main-is ver2src:exe:reduce-exe
2. > main
-}
module Main where
-- module Reduce where

{-
import Control.Arrow ((<<<))
import Debug.Trace (trace)
-}
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, _4, _5, _6, _7, (^.))
import Lib (TpConfmat, readFileUnavoidR, myLoop)


verts   = 27 -- max number of vertices in a free completion + 1
deg     = 13 -- max degree of a vertex in a free completion + 1
-- must be at least 13 because of row 0
edges   = 62 -- max number of edges in a free completion + 1
maxring = 14 -- max ring-size

type TpAngle  = [[Int]]
type TpEdgeno = [[Int]]


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"

  graphs <- readFileUnavoidR

  mainLoop graphs

  -- putStrLn "633個の好配置は全て、Ｄ可約orＣ可約です"
  putStrLn "プログラムは正常終了しました"

mainLoop :: [TpConfmat] -> IO ()
mainLoop graphs
  | null graphs = return ()
  | otherwise   =
    let
      graph = head graphs
      {- "findangles" fills in the arrays "angle","diffangle","sameangle" and
         "contract" from the input "graph". "angle" will be used to compute
         which colourings of the ring edges extend to the configuration; the
         others will not be used unless a contract is specified, and if so
         they will be used in "checkcontract" below to verify that the
         contract is correct. -}
      (angle, diffangle, sameangle, contract) = findangles graph
    in print graph


{- writes into angle[i] all edges with number >i on a common triangle T say
   with edge i; and if there is a contract X given, and i is not in X, writes
   into diffangle[i] all such edges such that no edge of T is in X, and
   writes into sameangle[i] all such edges not in X so that the third edge of
   T is in X. Sets contract[i] to 1 if edge number i is in X and to zero
   otherwise, checks that X is sparse, and if |X|=4 checks that X has a triad -}
findangles :: TpConfmat -> (TpAngle, TpAngle, TpAngle, [Int])
findangles graph =
  let
    edge   = 3 * ((graph ^?! ix 0) ^?! ix 0) - 3 - ((graph ^?! ix 0) ^?! ix 1)
    edgeno = strip graph
  in ([[]], [[]], [[]], [])

{- Numbers edges from 1 up, so that each edge has as many later edges in
   triangles as possible; the ring edges are first.  edgeno[u][v] will be the
   number of the edge with ends u,v if there is such an edge and 0 otherwise. -}
strip :: TpConfmat -> TpEdgeno
strip graph =
  let
    verts = (graph ^?! ix 0) ^?! ix 0
    ring  = (graph ^?! ix 0) ^?! ix 1
  in [[]]

-- if grav meets the done vertices in an interval of length >=1, it returns
-- the length of the interval, and otherwise returns 0
inInterval :: [Int] -> [Bool] -> Int
inInterval grav done =
  let
    d     = head grav
    first = fromJust
      $ find (\x -> (x < d) && not (done !! (grav !! x))) [1 .. (deg - 1)]
  in
    if first == d
      then fromEnum $ done !! (grav !! d)
      else
        let last = fromJust $ find
              (\x -> (x < d) && not (done !! (grav !! (x + 1))))
              [first .. (deg - 1)]
            len  = last - first + 1
        in  if last == d
              then len
              else if first > 1
                then if any (\x -> done !! (grav !! x)) [(last + 2) .. d]
                  then 0
                  else len
                else
                  let
                    f n@(retN, _, _) x cont
                      | retN == 0 = n
                      | otherwise = cont $ inIntervalSub grav done n x
                    (retN, l, _) =
                      myLoop f id (1, len, False) [(last + 2) .. d]
                  in
                    if retN == 0 then 0 else l

inIntervalSub :: [Int] -> [Bool] -> (Int, Int, Bool) -> Int -> (Int, Int, Bool)
inIntervalSub grav done (_, l, w) j
  | done !! (grav !! j) = (1, l + 1, True)
  | w                   = (0, l,     w   )
  | otherwise           = (1, l,     w   )


