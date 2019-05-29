{-
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「present7」「rules」「unavoidable.conf」を取得し、
    本ファイルと同じ場所に置く。
2. > stack install lens --resolver lts
3. > stack exec ghci --resolver lts
4. > :l Discharge
5. > main
6. > 「7」を入力してEnter。
7. 中心の次数7のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK
-}
module Discharge where

import Data.List (find)
import Data.Maybe (fromJust, isJust)

import Control.Arrow ((<<<))
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, (^.))

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
-- import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
-- import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray)


verts = 27                -- max number of vertices in a free completion + 1

maxval = 12
cartvert = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

infty = 12                -- the "12" in the definition of limited part
maxoutlets = 110          -- max number of outlets

maxelist = 134            -- length of edgelist[a][b]

maxstack = 5              -- max height of Astack (see "Reduce")
maxlev = 12               -- max level of an input line + 1

-- type TpConfmat = IOUArray (Int, Int) Int
type TpAxle       = ([[Int]], [[Int]], Int)
type TpCond       = ([Int], [Int])
type TpAdjmat     = [[Int]]
type TpVertices   = ([[Int]], Int)
type TpQuestion   = ([Int], [Int], [Int], [Int])
type TpEdgelist   = [[[Int]]]
type TpPosout     = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]], [Int])
type TpReducePack = (TpAxle, [Int], [Int], TpAdjmat, TpEdgelist, [Bool], TpVertices, [TpQuestion], TpVertices)

main :: IO ()
main = do
  putStrLn "これは四色定理の放電法をおこなうプログラムです"
  putStrLn "中心の次数7,8,9,10,11のいずれかを入力してください"
  degStr <- getLine
  let deg = read degStr :: Int

  -- TpAxle
  let axles0 = replicate maxlev $ replicate cartvert 0
  let axlesLow = take (maxlev + 1) ([deg] ++ replicate (5*deg) 5     ++ repeat 0) : axles0
  let axlesUpp = take (maxlev + 1) ([deg] ++ replicate (5*deg) infty ++ repeat 0) : axles0

  -- CheckHubcap(axles, NULL, 0, print); -- read rules, compute outlets
  -- (void) Reduce(NULL, 0, 0);          -- read unavoidable set

  -- TpCond
  let nn = replicate maxlev 0
  let mm = replicate maxlev 0

  -- TpOutlet & TpPosout
  let number  = replicate (2 * maxoutlets) 0
  let nolines = replicate (2 * maxoutlets) 0
  let value   = replicate (2 * maxoutlets) 0
  let pos     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let low     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let upp     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let xx      = replicate (2 * maxoutlets) 0

  inStr <- readFile $ "present" ++ degStr
  ret <- mainLoop (number, nolines, value, pos, low, upp, xx) (nn, mm) deg 0 (axlesLow, axlesUpp, 0) (tail (map words (lines inStr)))

  -- final check
  if ret == "Q.E.D." then
    putStrLn $ "中心の次数" ++ degStr ++ "のグラフは、電荷が負になるか、近くに好配置があらわれるかです"
  else
    putStr ""
  putStrLn "プログラムは正常終了しました"


mainLoop :: TpPosout -> TpCond -> Int -> Int -> TpAxle -> [[String]] -> IO String
mainLoop posout (nn, mm) deg nosym axles@(low, upp, lev) tactics
  | lev >= maxlev = error "More than %d levels"
  | lev < 0       = return $ head $ head tactics
  | otherwise =
    case head tactics !! 1 of
      "S" -> do
              putStrLn "Symmetry"
              checkSymmetry (tail (tail (head tactics))) (low, upp, lev) posout nosym
              mainLoop posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "R" -> do
              putStrLn "Reduce"
              --if (Reduce(A, lineno, print >= PRTBAS ? 1 : 0) != 1) then Error("Reducibility failed", lineno);
              mainLoop posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "H" -> do
              putStrLn "Hubcap"
              -- checkHubcap posout (tail (tail (head tactics))) (low, upp, lev)
              mainLoop posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "C" -> do
              putStrLn "Condition"
              let n = read (head tactics !! 2) :: Int
              let m = read (head tactics !! 3) :: Int
              nosym2                   <- checkCondition1 (nn, mm) deg (low, upp, lev) n m nosym
              (cond2, (low2, upp2, _)) <- checkCondition2 (nn, mm) (low, upp, lev) n m
              mainLoop posout cond2 deg nosym2 (low2, upp2, lev+1) (tail tactics)
      _   -> error "Invalid instruction"


checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, _, lev) posout@(number, nolines, _, _, _, _, _) nosym =
  let [k, epsilon, level, line] = map read str :: [Int];
      i = fromJust $ find (==line) number
  in if k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1 then error "Illegal symmetry"
     else if i >= nosym then                                              error "No symmetry as requested"
          else if nolines !! i /= level + 1 then                          error "Level mismatch"
               else if epsilon == 0 && outletForced aA /= 1 then          error "Invalid symmetry"
                    else if reflForced aA /= 1 then                       error "Invalid reflected symmetry"
                                               else                       putStrLn "checkSymmetry OK."


outletForced :: TpAxle -> Int
outletForced _ = 1


reflForced :: TpAxle -> Int
reflForced _ = 1


checkCondition1 :: TpCond -> Int -> TpAxle -> Int -> Int -> Int -> IO Int
checkCondition1 (nn, mm) deg aA@(low, upp, lev) n m nosym =
  let ret = find (\x-> 1 <= x && x <= 2*deg) nn
  in if isJust ret then return (nosym+1) else return nosym


checkCondition2 :: TpCond -> TpAxle -> Int -> Int -> IO (TpCond, TpAxle)
checkCondition2 (nn, mm) aA@(low, upp, lev) n m =
  let low2 = low & ix (lev+1) .~ (low ^?! ix lev);
      upp2 = upp & ix (lev+1) .~ (upp ^?! ix lev);
      aLowN = (low2 ^?! ix lev) ^?! ix n;
      aUppN = (upp2 ^?! ix lev) ^?! ix n
  in if m > 0
    then -- new lower bound
      if aLowN >= m || m > aUppN
        then
          error "Invalid lower bound in condition"
        else
          let upp3 = upp2 & (ix lev     <<< ix n) .~ (m-1);
              low3 = low2 & (ix (lev+1) <<< ix n) .~ m;
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0;
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))
    else -- new upper bound
      if aLowN > -m || -m >= aUppN
        then
          error "Invalid upper bound in condition"
        else
          let low3 = low2 & (ix lev     <<< ix n) .~ (1-m);
              upp3 = upp2 & (ix (lev+1) <<< ix n) .~ (-m);
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0;
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))


myOr :: Bool -> Bool -> Int -> (Bool, Int)
myOr False False _ = (False, 0)
myOr False True  n = (True,  n+1)
myOr True  _     n = (True,  n)
myAny :: (a -> Bool) -> [a] -> Int -> (Bool, Int)
myAny _ []     _ = (False, 0)
myAny p (x:xs) n = myOr (p x) (fst (myAny p xs (n+1))) n


reduce :: TpReducePack -> TpAxle -> IO Bool
reduce rP@(aStack@(aSLow, aSUpp, aSLev), bLow, bUpp, adjmat, edgelist, used, image, redquestions, vert) aA@(low, upp, lev) =
  let aStack2Low = aSLow & ix 0 .~ (low ^?! ix lev);
      aStack2Upp = aSUpp & ix 0 .~ (upp ^?! ix lev);
      aStack2    = (aStack2Low, aStack2Upp, aSLev)
  in reduceSub (rP & _1 .~ aStack2) aA 1
reduceSub :: TpReducePack -> TpAxle -> Int -> IO Bool
reduceSub rP@(aStack@(aSLow, aSUpp, aSLev), bLow, bUpp, adjmat, edgelist, used, image, redquestions, vert) aA naxles
  | naxles <= 0 = do {putStrLn "All possibilities for lower degrees tested"; return True}
  | otherwise   = do
    let noconf = 633 -- 好配置の個数
    let bLow2 = aSLow !! (naxles - 1);
        bUpp2 = aSUpp !! (naxles - 1)
    let (bLow3, bUpp3) = getadjmat   (bLow2, bUpp2) adjmat
    let (bLow4, bUpp4) = getEdgelist (bLow3, bUpp3) edgelist

    -- subConfが一度も成功しなかったら、Not reducible
    --let (retB, retH) = myAny (subConf used adjmat bUpp4 edgelist 0 image 1) [redquestions !! h | h <- [0..(noconf-1)]] 0
    let (retB, retH) = myAny (subConf used adjmat vert edgelist 0 image 1) [redquestions !! h | h <- [0..(noconf-1)]] 0
    if retB
      then do
        -- Semi-reducibility test found h-th configuration, say K, appearing
        let redverts = ((redquestions !! retH) ^. _1) !! 1
        let redring  = ((redquestions !! retH) ^. _2) !! 1
        -- the above are no vertices and ring-size of free completion of K
        -- could not use conf[h][0][0], because conf may be NULL
        reduceSub rP aA $ getReduceN (redring + 1) redverts (naxles - 1) image
      else do
        putStrLn "Not reducible"
        return False


getReduceN :: Int -> Int -> Int -> TpVertices -> Int
getReduceN i redverts naxles image
  | i > redverts = naxles
  | otherwise    =
    let v = 0 --image !! i
    in if True --B->low[v] == B->upp[v]
      then
        getReduceN (i + 1) redverts naxles image
      else
        if naxles >= maxstack
          then
            error "More than %d elements in axle stack needed"
          else
            --CopyAxle(Astack[naxles4V], B);
            --Astack[naxles4V]->upp[v] = B->upp[v] - 1;
            getReduceN (i + 1) redverts (naxles + 1) image


getadjmat :: ([Int], [Int]) -> TpAdjmat -> ([Int], [Int])
getadjmat b adjmat = b
getEdgelist :: ([Int], [Int]) -> TpEdgelist -> ([Int], [Int])
getEdgelist b edgelist = b


--       pedgeHead <- readArray edgelist (qXi0, qXi1, 0)
--    static int used[cartvert]
subConf :: [Bool] -> TpAdjmat -> TpVertices -> TpEdgelist -> Int -> TpVertices -> Int -> TpQuestion -> Bool
subConf used adjmat (degree, dI) edgelist pedgeHead image i question@(qU, qV, qZ, qXi) = True



