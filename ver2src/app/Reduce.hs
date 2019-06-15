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
import Debug.Trace (trace)
-}
import Control.Arrow            ((<<<))
import Control.Lens             ((&), (.~), ix, (^?!), _1, _2, _4, _5, _6, _7, (^.))
import Control.Monad.Trans.Cont (Cont, cont, runCont, callCC)
import Data.List                (find)
import Data.Maybe               (fromJust, isJust)
import Lib                      (TpConfmat, readFileUnavoidR, foldlCont)


mVerts  = 27 -- max number of vertices in a free completion + 1
deg     = 13 -- max degree of a vertex in a free completion + 1
-- must be at least 13 because of row 0
edges   = 62 -- max number of edges in a free completion + 1
maxring = 14 -- max ring-size

type TpAngle  = [[Int]]
type TpEdgeno = [[Int]]
type TpF4     = ((Bool, Int, TpEdgeno) -> Cont (Bool, Int, TpEdgeno) (Bool, Int, TpEdgeno)) -> (Bool, Int, TpEdgeno) -> Int -> Cont (Bool, Int, TpEdgeno) (Bool, Int, TpEdgeno)

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
    verts                   = (graph ^?! ix 0) ^?! ix 0
    ring                    = (graph ^?! ix 0) ^?! ix 1
    edgeno0                 = replicate edges $ replicate edges 0
    edgeno1                 = (edgeno0 & (ix ring <<< ix 1) .~ 1) & (ix 1 <<< ix ring) .~ 1
    f1 n x                  = return $ stripSub1 n x
    edgeno2                 = runCont (foldlCont f1 edgeno1 [2 .. ring]) id
    done0                   = replicate mVerts False
    term0                   = 3 * (verts - 1) - ring
    f2 n x                  = return $ stripSub2 graph verts ring n x
    (edgeno3, done1, term1) = runCont (foldlCont f2 (edgeno2, done0, term0) [(ring + 1) .. verts]) id
    f3 n x                  = return $ stripSub3 graph verts ring n x
    (edgeno4, done2, term2) = runCont (foldlCont f3 (edgeno3, done1, term1) [1 .. ring]) id
  in [[]]

stripSub3Sub1 :: TpConfmat -> [Bool] -> Int -> (Int, Int) -> Int -> (Int, Int)
stripSub3Sub1 graph done ring (maxint, best) v =
  if done !! v
    then (maxint, best)
    else
      let
        u     = if v > 1    then v - 1 else ring
        w     = if v < ring then v + 1 else 1
        inter = 3 * (graph ^?! ix v ^?! ix 1) + 4 * (fromEnum (done !! u) + fromEnum (done !! w))
      in  if inter > maxint
            then (inter,  v   )
            else (maxint, best)

-- Now we must list the edges between the interior and the ring
stripSub3 :: TpConfmat -> Int -> Int -> (TpEdgeno, [Bool], Int) -> Int -> (TpEdgeno, [Bool], Int)
stripSub3 graph verts ring (edgeno0, done0, term0) x =
  let
    f1 n x                = return $ stripSub3Sub1 graph done0 ring n x
    (maxint, best)        = runCont (foldlCont f1 (0, 0) [1 .. ring]) id
{-    f2 n x cont           = cont $ stripSub2Sub2 graph max1 n x
    (maxdeg, hoge)        = myLoop f2 id (0, 0) [1 .. maxes]
    grav                  = graph !! best
    d                     = grav !! 1
    f3 n@(retB, _, _, p2) x cont
      | not p2    = n
      | retB      = n
      | otherwise = cont $ stripSub2Sub3 grav done0 d n x
    (_, first, _, _)      = myLoop f3 id (False, 1, done0 !! (grav !! (d + 1)), True) [0 .. 64]
    f4 n@(retB, _, _) x cont
      | retB      = n
      | otherwise = cont $ stripSub2Sub4 grav done0 d best first f4 n x
    (_, term1, edgeno1)   = myLoop f4 id (False, term0, edgeno0) [first .. 64] -}
  in (edgeno0, done0 & ix best .~ True, term0)

stripSub1 :: TpEdgeno -> Int -> TpEdgeno
stripSub1 edgeno v = (edgeno & (ix (v - 1) <<< ix v) .~ v) & (ix v <<< ix (v - 1)) .~ v

-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
stripSub2 :: TpConfmat -> Int -> Int -> (TpEdgeno, [Bool], Int) -> Int -> (TpEdgeno, [Bool], Int)
stripSub2 graph verts ring (edgeno0, done0, term0) x =
  let
    max0                  = replicate mVerts 0
    f1 n x                = return $ stripSub2Sub1 graph done0 n x
    (maxint, maxes, max1) = runCont (foldlCont f1 (0, 0, max0) [(ring + 1) .. verts]) id
    f2 n x                = return $ stripSub2Sub2 graph max1 n x
    (maxdeg, best)        = runCont (foldlCont f2 (0, 0) [1 .. maxes]) id
    grav                  = graph !! best
    d                     = grav !! 1
    f3 exit n@(retB, _, _, p2) x
      | not p2    = exit n
      | retB      = exit n
      | otherwise = return $ stripSub2Sub3 grav done0 d n x
    (_, first, _, _)      = runCont (callCC $ \exit -> foldlCont (f3 exit) (False, 1, done0 !! (grav !! (d + 1)), True) [0 .. 64]) id
    f4 exit n@(retB, _, _) x
      | retB      = exit n
      | otherwise = return $ stripSub2Sub4 grav done0 d best first f4 n x
    (_, term1, edgeno1)   = runCont (callCC $ \exit -> foldlCont (f4 exit) (False, term0, edgeno0) [first .. 64]) id
  in (edgeno1, done0 & ix best .~ True, term1)

stripSub2Sub1 :: TpConfmat -> [Bool] -> (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
stripSub2Sub1 graph done (maxint, maxes, max) v =
  if done !! v
    then (maxint, maxes, max)
    else
      let inter = inInterval (graph !! v) done
      in  if inter > maxint
            then   (inter,  1,         max & ix 1 .~ v          )
            else if inter == maxint
              then (maxint, maxes + 1, max & ix (maxes + 1) .~ v)
              else (maxint, maxes,     max                      )

-- From the terms in max we choose the one of maximum degree
stripSub2Sub2 :: TpConfmat -> [Int] -> (Int, Int) -> Int -> (Int, Int)
stripSub2Sub2 graph max (maxdeg, best) h =
  let
    d = (graph ^?! ix (h + 1)) ^?! ix 0
  in if d > maxdeg then (d, max !! h) else (maxdeg, best)

-- So now, the vertex "best" will be the next vertex to be done
stripSub2Sub3 :: [Int] -> [Bool] -> Int -> (Bool, Int, Bool, Bool) -> Int -> (Bool, Int, Bool, Bool)
stripSub2Sub3 grav done d (_, first0, _, _) _ =
  let first1   = first0 + 1
      previous = done !! (grav !! (first1 + 1))
  in  if first0 > d then (True, 1, previous, True)
      else if not previous && done !! (grav !! (first0 + 1)) then (False, first1, previous, False) else (False, first1, previous, True)

-- This eventually lists all the internal edges of the configuration
stripSub2Sub4 :: [Int] -> [Bool] -> Int -> Int -> Int -> TpF4 -> (Bool, Int, TpEdgeno) -> Int -> (Bool, Int, TpEdgeno)
stripSub2Sub4 grav done d best first f4 (_, term0, edgeno0) h =
  if not $ done !! (grav !! (h + 1))
    then (True, term0, edgeno0)
    else
      let edgeno1 = (edgeno0 & (ix best <<< ix (grav !! (h + 1))) .~ term0) & (ix (grav !! (h + 1)) <<< ix best) .~ term0
          term1   = term0 - 1
      in  if h == d
            then if first == 1
              then (True, term1, edgeno1)
              else runCont (callCC $ \exit -> foldlCont (f4 exit) (False, term1, edgeno1) [0 .. 64]) id
            else (False, term1, edgeno1)

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
                    f exit n@(retN, _, _) x
                      | retN == 0 = exit n
                      | otherwise = return $ inIntervalSub grav done n x
                    (retN, l, _) =
                      runCont (callCC $ \exit -> foldlCont (f exit) (1, len, False) [(last + 2) .. d]) id
                  in
                    if retN == 0 then 0 else l

inIntervalSub :: [Int] -> [Bool] -> (Int, Int, Bool) -> Int -> (Int, Int, Bool)
inIntervalSub grav done (_, l, w) j
  | done !! (grav !! j) = (1, l + 1, True)
  | w                   = (0, l,     w   )
  | otherwise           = (1, l,     w   )



