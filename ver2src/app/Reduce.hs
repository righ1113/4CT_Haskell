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
import Control.Arrow             ((<<<))
import Control.Lens              ((&), (.~), ix, (^?!), (+~), (-~), (*~), _3, _4, _5, _6, _7)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont  (Cont, cont, runCont, callCC, ContT(..))
import Data.Bits                 ((.&.), (.|.))
import Data.List                 (find)
import Data.Maybe                (fromJust, isJust)
import Lib                       (TpConfmat, readFileUnavoidR, foldlCont)
--import Control.Monad.IO.Class (liftIO)


mVerts        = 27 -- max number of vertices in a free completion + 1
deg           = 13 -- max degree of a vertex in a free completion + 1
-- must be at least 13 because of row 0
edges         = 62 -- max number of edges in a free completion + 1
maxring       = 14 -- max ring-size
power         = [0, 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907] -- 3^(i-1)
simatchnumber = [0, 0, 1, 3, 10, 30, 95, 301, 980, 3228, 10797, 36487, 124542, 428506, 1485003]

type TpAngle        = [[Int]]
type TpEdgeno       = [[Int]]
type TpF4           = ((Bool, Int, TpEdgeno) -> Cont (Bool, Int, TpEdgeno) (Bool, Int, TpEdgeno)) -> (Bool, Int, TpEdgeno) -> Int -> Cont (Bool, Int, TpEdgeno) (Bool, Int, TpEdgeno)
type TpFindlivePack = (Bool, Int, Int, [Int], [Int], Int, [Int])


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
  | otherwise   = do

    let graph = head graphs
    {- "findangles" fills in the arrays "angle","diffangle","sameangle" and
        "contract" from the input "graph". "angle" will be used to compute
        which colourings of the ring edges extend to the configuration; the
        others will not be used unless a contract is specified, and if so
        they will be used in "checkcontract" below to verify that the
        contract is correct. -}
    (angle, diffangle, sameangle, contract) <- findangles graph
    --print contract
    --print graph

    let ring   = (graph ^?! ix 0) ^?! ix 1   -- ring-size
        ncodes = (power !! ring + 1) `div` 2 -- number of codes of colorings of R
        live0  = replicate ncodes 1
        real0  = replicate (simatchnumber !! maxring `div` 8 + 2) 255
        nchar  = simatchnumber !! ring `div` 8 + 1
    (nlive1, live1) <- findlive live0 ncodes angle power ((graph ^?! ix 0) ^?! ix 2)

    -- computes {\cal M}_{i+1} from {\cal M}_i, updates the bits of "real"
    (nlive2, live2) <- updatelive ring real0 power live0 nchar ncodes 1 -- bug4 live1 nchar ncodes nlive1
    -- computes {\cal C}_{i+1} from {\cal C}_i, updates "live"

    mainLoop [] -- $ tail graphs


-- ###################################################################################################################################
-- ###################################################################################################################################
updatelive :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> IO (Int, [Int])
updatelive ring real0 power live1 nchar ncodes nlive1 = do
  let
    f1 exit n@(retB, _, _) x
      | retB      = exit n
      | otherwise = updateliveSub ring real0 power nchar ncodes n x
  (retB, nlive2, live2) <- runContT (callCC $ \exit -> foldlCont (f1 exit) (False, nlive1, live1) [0..1023]) return
  if retB then return (nlive2, live2)
          else error "updatelive : It was not good though it was repeated 1024 times!"

updateliveSub :: Int -> [Int] -> [Int] -> Int -> Int -> (Bool, Int, [Int]) -> Int -> ContT (Bool, Int, [Int]) IO (Bool, Int, [Int])
updateliveSub ring real0 power nchar ncodes n@(retB, nlive, live1) _ = do
  lift $ testmatch ring real0 power live1 nchar

  let
    newnlive    = 0
    live2       = if head live1 > 1 then live1 & ix 0 .~ 15 else live1
    f1 exit n x = updateliveSubSub n x

  ret@(live3, newnlive2) <- lift $ runContT (callCC $ \exit -> foldlCont (f1 exit) (live2, newnlive) [0..(ncodes-1)]) return

  if (newnlive2 < nlive) && (newnlive2 > 0)
    then return (False, nlive, live3)
    else do
      if newnlive2 == 0
        then lift $ putStrLn "\n\n\n                  ***  D-reducible  ***\n"
        else lift $ putStrLn "\n\n\n                ***  Not D-reducible  ***"
      return (True, nlive, live3)

updateliveSubSub :: ([Int], Int) -> Int -> ContT ([Int], Int) IO ([Int], Int)
updateliveSubSub (live, newnlive) i =
  if live !! i /= 15
    then return (live & ix i .~ 0, newnlive)
    else return (live & ix i .~ 1, newnlive + 1)

testmatch :: Int -> [Int] -> [Int] -> [Int] -> Int -> IO ()
testmatch ring real0 power live1 nchar = do
  let nreal = 0
  putStrLn $ "               " ++ show nreal


-- ###################################################################################################################################
-- ###################################################################################################################################
{- writes into angle[i] all edges with number >i on a common triangle T say
   with edge i; and if there is a contract X given, and i is not in X, writes
   into diffangle[i] all such edges such that no edge of T is in X, and
   writes into sameangle[i] all such edges not in X so that the third edge of
   T is in X. Sets contract[i] to 1 if edge number i is in X and to zero
   otherwise, checks that X is sparse, and if |X|=4 checks that X has a triad -}
findangles :: TpConfmat -> IO (TpAngle, TpAngle, TpAngle, [Int])
findangles graph =
  let
    edge                             = 3 * ((graph ^?! ix 0) ^?! ix 0) - 3 - ((graph ^?! ix 0) ^?! ix 1)
    edgeno                           = strip graph
    contract0                        = replicate (edges + 1) 0
    contract1                        = contract0 & ix 0     .~ ((graph ^?! ix 1) ^?! ix 0)
    contract2                        = contract1 & ix edges .~ ((graph ^?! ix 0) ^?! ix 3)
    f1 n x                           = return $ findanglesSub1 graph edgeno n x
    contract3                        = runCont (foldlCont f1 contract2 [1 .. (head contract2)]) id
    angle0                           = replicate edges $ replicate 5 0
    diffangle0                       = replicate edges $ replicate 5 0
    sameangle0                       = replicate edges $ replicate 5 0
    angle1                           =     angle0 & (ix 0 <<< ix 0) .~ ((graph ^?! ix 0) ^?! ix 0)
    angle2                           =     angle1 & (ix 0 <<< ix 1) .~ ((graph ^?! ix 0) ^?! ix 1)
    angle3                           =     angle2 & (ix 0 <<< ix 2) .~ edge
    diffangle1                       = diffangle0 & (ix 0 <<< ix 0) .~ ((graph ^?! ix 0) ^?! ix 0)
    diffangle2                       = diffangle1 & (ix 0 <<< ix 1) .~ ((graph ^?! ix 0) ^?! ix 1)
    diffangle3                       = diffangle2 & (ix 0 <<< ix 2) .~ edge
    f2 n x                           = return $ findanglesSub2 graph edgeno contract3 n x
    (angle4, diffangle4, sameangle1) = runCont (foldlCont f2 (angle3, diffangle3, sameangle0) [(v, h) | v <- [1 .. ((graph ^?! ix 0) ^?! ix 0)], h <- [1 .. ((graph ^?! ix (v + 1)) ^?! ix 1)]]) id
  in if head contract3 < 4 -- checking that there is a triad
    then return (angle4, diffangle4, sameangle1, contract3)
    else let
      f3 exit n x
        | n         = exit n
        | otherwise = return $ findanglesSub3 graph n x
      retB          = runCont (callCC $ \exit -> foldlCont (f3 exit) False [(1 + head graph !! 1) .. (head $ head graph)]) id
        in if retB then return (angle4, diffangle4, sameangle1, contract3)
                   else error "***  ERROR: CONTRACT HAS NO TRIAD  ***"

findanglesSub1 :: TpConfmat -> TpEdgeno -> [Int] -> Int -> [Int]
findanglesSub1 graph edgeno contract2 i =
  let
    u = (graph ^?! ix 1) ^?! ix (i * 2 - 1)
    v = (graph ^?! ix 1) ^?! ix (i * 2)
  in
    contract2 & ix ((edgeno ^?! ix u) ^?! ix v) .~ 1

findanglesSub2 :: TpConfmat -> TpEdgeno -> [Int] -> (TpAngle, TpAngle, TpAngle) -> (Int, Int) -> (TpAngle, TpAngle, TpAngle)
findanglesSub2 graph edgeno contract (angle3, diffangle3, sameangle0) (v, h) = 
  if (v <= (graph ^?! ix 0) ^?! ix 1) && (h == (graph ^?! ix (v + 1)) ^?! ix 1)
    then
      (angle3, diffangle3, sameangle0)
    else let
      i = if h < (graph ^?! ix (v + 1)) ^?! ix 1 then h + 1 else 1
      u = (graph ^?! ix (v + 1)) ^?! ix (h + 1)
      w = (graph ^?! ix (v + 1)) ^?! ix (i + 1)
      a = (edgeno ^?! ix v) ^?! ix w
      b = (edgeno ^?! ix u) ^?! ix w
      c = (edgeno ^?! ix u) ^?! ix v
        in if a > c
          then let
            angle4 = angle3 & (ix c <<< ix (1 + (angle3 ^?! ix c) ^?! ix 0)) .~ a
              in if (0 == contract !! a) && (0 == contract !! b) && (0 == contract !! c)
                then let
                  diffangle4 = diffangle3 & (ix c <<< ix (1 + (diffangle3 ^?! ix c) ^?! ix 0)) .~ a
                  in if 0 /= contract !! b
                    then let
                      sameangle1 = sameangle0 & (ix c <<< ix (1 + (diffangle3 ^?! ix c) ^?! ix 0)) .~ a
                      in   (angle4, diffangle4, sameangle1)
                    else   (angle4, diffangle4, sameangle0)
                else       (angle4, diffangle3, sameangle0)
          else if b > c
            then let
              angle4 = angle3 & (ix c <<< ix (1 + (angle3 ^?! ix c) ^?! ix 0)) .~ b
                in if (0 == contract !! a) && (0 == contract !! b) && (0 == contract !! c)
                  then let
                    diffangle4 = diffangle3 & (ix c <<< ix 0) .~ b
                    in if 0 /= contract !! b
                      then let
                        sameangle1 = sameangle0 & (ix c <<< ix 0) .~ b
                        in (angle4, diffangle4, sameangle1)
                      else (angle4, diffangle4, sameangle0)
                  else     (angle4, diffangle3, sameangle0)
            else           (angle3, diffangle3, sameangle0)

findanglesSub3 :: TpConfmat -> Bool -> Int -> Bool
findanglesSub3 graph _ v =
  (a >= 3) && (((graph !! (v + 1)) !! 1) >= 6) || retB
    where
      f1 n x     = return $ findanglesSub3Sub1 graph v n x
      a          = runCont (foldlCont f1 0 [1 .. ((graph !! (v + 1)) !! 1)]) id
      neighbour0 = replicate (head (head graph)) False
      f2 n x     = return $ findanglesSub3Sub2 graph v n x
      neighbour1 = runCont (foldlCont f2 neighbour0 [1 .. ((graph !! (v + 1)) !! 1)]) id
      f3 n x     = return $ findanglesSub3Sub3 graph neighbour1 n x
      retB       = runCont (foldlCont f3 False [0 .. (length (graph !! 1) - 1)]) id

findanglesSub3Sub1 :: TpConfmat -> Int -> Int -> Int -> Int
findanglesSub3Sub1 graph v a i =
  let
    u    = (graph !! (v + 1)) !! (i + 1)
    retM = find (\x -> u == ((graph !! 1) !! x)) [0 .. (length (graph !! 1) - 1)]
  in if isJust retM then a + 1
                    else a

findanglesSub3Sub2 :: TpConfmat -> Int -> [Bool] -> Int -> [Bool]
findanglesSub3Sub2 graph v neighbour i = neighbour & ix ((graph ^?! ix (v + 1)) ^?! ix (i + 1)) .~ True

findanglesSub3Sub3 :: TpConfmat -> [Bool] -> Bool -> Int -> Bool
findanglesSub3Sub3 graph neighbour retB j = retB || not (neighbour !! (head graph !! j))


-- ###################################################################################################################################
-- ###################################################################################################################################
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
    (edgeno4, _,     _    ) = runCont (foldlCont f3 (edgeno3, done1, term1) [1 .. ring]) id
  in edgeno2 --edgeno4      !!!bug1!!!

stripSub1 :: TpEdgeno -> Int -> TpEdgeno
stripSub1 edgeno v = (edgeno & (ix (v - 1) <<< ix v) .~ v) & (ix v <<< ix (v - 1)) .~ v

-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
stripSub2 :: TpConfmat -> Int -> Int -> (TpEdgeno, [Bool], Int) -> Int -> (TpEdgeno, [Bool], Int)
stripSub2 graph verts ring (edgeno0, done0, term0) x =
-- bug1有り *** Exception: Prelude.!!: index too large
  let
    max0                  = replicate mVerts 0
    f1 n x                = return $ stripSub2Sub1 graph done0 n x
    (maxint, maxes, max1) = runCont (foldlCont f1 (0, 0, max0) [(ring + 1) .. verts]) id
    f2 n x                = return $ stripSub2Sub2 graph max1 n x
    (maxdeg, best)        = runCont (foldlCont f2 (0, 0) [1 .. maxes]) id
    grav                  = graph !! (best + 1)
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
      let inter = inInterval (graph !! (v + 1)) done
      in  if inter > maxint
            then   (inter,  1,         max & ix 1 .~ v          )
            else if inter == maxint
              then (maxint, maxes + 1, max & ix (maxes + 1) .~ v)
              else (maxint, maxes,     max                      )

-- if grav meets the done vertices in an interval of length >=1, it returns
-- the length of the interval, and otherwise returns 0
inInterval :: [Int] -> [Bool] -> Int
inInterval grav done =
  let
    d     = grav !! 1
    first = fromJust
      $ find (\x -> (x < d) && not (done !! (grav !! (x + 1)))) [1 .. (deg - 1)]
  in
    if first == d
      then fromEnum $ done !! (grav !! (d + 1))
      else
        let last = fromJust $ find
              (\x -> (x < d) && not (done !! (grav !! (x + 1))))
              [first .. (deg - 1)]
            len  = last - first + 1
        in  if last == d
              then len
              else if first > 1
                then if any (\x -> done !! (grav !! (x + 1))) [(last + 2) .. d]
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
  | done !! (grav !! (j + 1)) = (1, l + 1, True)
  | w                         = (0, l,     w   )
  | otherwise                 = (1, l,     w   )

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

-- Now we must list the edges between the interior and the ring
stripSub3 :: TpConfmat -> Int -> Int -> (TpEdgeno, [Bool], Int) -> Int -> (TpEdgeno, [Bool], Int)
stripSub3 graph verts ring (edgeno0, done0, term0) x =
  let
    f1 n x                = return $ stripSub3Sub1 graph done0 ring n x
    (maxint, best)        = runCont (foldlCont f1 (0, 0) [1 .. ring]) id
    grav                  = graph !! (best + 1)
    u                     = if best > 1 then best - 1 else ring
    f2 n x                = return $ stripSub3Sub2 best grav n x
  in if done0 !! u
    then let
      (edgeno1, term1) = runCont (foldlCont f2 (edgeno0, term0) [((grav !! 1) - 1), ((grav !! 1) - 2) .. 1]) id
        in (edgeno1, done0 & ix best .~ True, term1)
    else let
      (edgeno1, term1) = runCont (foldlCont f2 (edgeno0, term0) [2 .. ((grav !! 1) - 1)                   ]) id
        in (edgeno1, done0 & ix best .~ True, term1)

stripSub3Sub1 :: TpConfmat -> [Bool] -> Int -> (Int, Int) -> Int -> (Int, Int)
stripSub3Sub1 graph done ring (maxint, best) v =
  if done !! v
    then (maxint, best)
    else
      let
        u     = if v > 1    then v - 1 else ring
        w     = if v < ring then v + 1 else 1
        inter = 3 * (graph ^?! ix (v + 1) ^?! ix 1) + 4 * (fromEnum (done !! u) + fromEnum (done !! w))
      in  if inter > maxint
            then (inter,  v   )
            else (maxint, best)

stripSub3Sub2 :: Int -> [Int] -> (TpEdgeno, Int) -> Int -> (TpEdgeno, Int)
stripSub3Sub2 best grav (edgeno0, term0) h =
  let edgeno1 = (edgeno0 & (ix best <<< ix (grav !! (h + 1))) .~ term0) & (ix (grav !! (h + 1)) <<< ix best) .~ term0
      term1   = term0 - 1
  in (edgeno1, term1)


-- ###################################################################################################################################
-- ###################################################################################################################################
{- computes {\cal C}_0 and stores it in live. That is, computes codes of
   colorings of the ring that are not restrictions of tri-colorings of the
   free extension. Returns the number of such codes -}
findlive :: [Int] -> Int -> TpAngle -> [Int] -> Int -> IO (Int, [Int])
findlive live ncodes angle power extentclaim = do
  let
    ring       = (angle ^?! ix 0) ^?! ix 1
    ed         = (angle ^?! ix 0) ^?! ix 2
    bigno      = (power !! (ring + 1) - 1) `div` 2 -- number of codes of colorings of R
    c0         = replicate edges 0
    c1         = c0 & ix ed .~ 1
    j          = ed - 1
    c2         = c1 & ix j .~ 2
    forbidden0 = replicate edges 0
    forbidden1 = forbidden0 & ix j .~ 5
    f1 exit n@(retB, _, _, _, _, _, _) x
      | retB      = exit n
      | otherwise = findliveSub bigno angle power ring ed extentclaim n x
  (retB, retN, _, _, _, _, retLive) <- runContT (callCC $ \exit -> foldlCont (f1 exit) (False, ncodes, j, c2, forbidden1, 0, live) [0..1023]) return
  if retB then return (retN, retLive)
          else error "findlive : It was not good though it was repeated 1024 times!"

findliveSub :: Int -> TpAngle -> [Int] -> Int -> Int -> Int -> TpFindlivePack -> Int -> ContT TpFindlivePack IO TpFindlivePack
findliveSub bigno angle power ring ed extentclaim n@(_, _, _, _, _, _, live) _ = do
  let
    f1 exit n@(retB, _, j, c, forbidden, _, _) x
      | retB                               = exit n
      | (forbidden !! j) .&. (c !! j) == 0 = exit n
      | otherwise                          = findliveSubSub1 ring ed extentclaim n x
  ret@(retB, _, j, c, forbidden, extent, _) <- lift $ runContT (callCC $ \exit -> foldlCont (f1 exit) n [0..1023]) return
  if retB
    then return ret
    else if j == ring + 1
      then
        let (extent1, live1) = record c power ring angle live extent bigno
        in findliveSubSub1 ring ed extentclaim ((ret & _6 .~ extent1) & _7 .~ live1) 0
      else
        let
          am         = angle !! (j - 1)
          c1         = c & ix (j - 1) .~ 1
          f2 n x     = return $ findliveSubSub2 am c1 n x
          u          = runCont (foldlCont f2 0 [i | i <- [1 .. 1023], i <= head am]) id
          forbidden1 = forbidden & ix (j - 1) .~ u
        in return (((ret & _3 -~ 1) & _4 .~ c1) & _5 .~ forbidden1)

{- Given a colouring specified by a 1,2,4-valued function "col", it computes
   the corresponding number, checks if it is in live, and if so removes it. -}
record :: [Int] -> [Int] -> Int -> TpAngle -> [Int] -> Int -> Int -> (Int, [Int])
record c power ring angle live extent bigno =
  let
    weight0      = replicate 5 0
    f1 n x       = return $ recordSub1 c power angle n x
    weight1      = runCont (foldlCont f1 weight0 [1 .. ring]) id
    min0         = weight1 !! 4
    max0         = weight1 !! 4
    f2 n x       = return $ recordSub2 weight1 n x
    (min1, max1) = runCont (foldlCont f2 (min0, max0) [1, 2]) id
    colno        = bigno - 2 * min1 - max1
  in if live !! colno /= 0
    then (extent + 1, live & ix colno .~ 0)
    else (extent    , live                )

recordSub1 :: [Int] -> [Int] -> TpAngle -> [Int] -> Int -> [Int]
recordSub1 c power angle weight i =
  let sum = 7 - c !! ((angle !! i) !! 1) - c !! ((angle !! i) !! 2)
  in weight & ix sum +~ (power !! i)

recordSub2 :: [Int] -> (Int, Int) -> Int -> (Int, Int)
recordSub2 weight (min, max) i =
  let w = weight !! i
  in if w < min
    then (w, max)
    else if w > max
      then (min, w)
      else (min, max)

findliveSubSub2 :: [Int] -> [Int] -> Int -> Int -> Int
findliveSubSub2 am c1 u i = u .|. c1 !! (am !! i)

findliveSubSub1 :: Int -> Int -> Int -> TpFindlivePack -> Int -> ContT TpFindlivePack IO TpFindlivePack
findliveSubSub1 ring ed extentclaim n@(_, _, j, c, _, _, _) _ = do
  let
    f1 exit n@(retB, _, j, c, _, _, _) x
      | retB                = exit n
--      | (c !! j) .&. 8 == 0 = exit n      !!!bug2!!!
      | otherwise           = findliveSubSubSub ring ed extentclaim n x
  ret@(retB, _, j1, c1, _, _, _) <- lift $ runContT (callCC $ \exit -> foldlCont (f1 exit) (n & _4 .~ (c & ix j *~ 2)) [0..1023]) return
  if retB || (c1 !! j1) .&. 8 == 0
    then return ret
    else error "findliveSubSub1 : It was not good though it was repeated 1024 times!"

findliveSubSubSub :: Int -> Int -> Int -> TpFindlivePack -> Int -> ContT TpFindlivePack IO TpFindlivePack
findliveSubSubSub ring ed extentclaim n@(retB, ncodes, j, c, forbidden, extent, live) _ =
  -- lift $ putStrLn $ "j, ed-1 : " ++ show j ++ ", " ++ show (ed-1)
  if j >= ed - 1
    then do
      lift $ printStatus ring ncodes extent extentclaim
      return (True, ncodes - extent, j, c, forbidden, extent, live)
    else
      --lift $ putStrLn "ihihi"
      return (retB, ncodes, j + 1, c & ix (j + 1) *~ 2, forbidden, extent, live)

printStatus :: Int -> Int -> Int -> Int -> IO ()
printStatus ring totalcols extent extentclaim = do
  putStr   $ "\n\n   This has ring-size " ++ show ring ++ ", so there are " ++ show totalcols ++ " colourings total,\n"
  putStr   $ "   and " ++ show (simatchnumber !! ring) ++ " balanced signed matchings.\n"
  -- putStr   $ "\n   There are " ++ show extent ++ " colourings that extend to the configuration."         !!!bug3!!!
  putStr     "\n\n            remaining               remaining balanced\n"
  putStr     "           colourings               signed matchings\n"
  -- putStrLn $ "\n              " ++ show (totalcols - extent)



