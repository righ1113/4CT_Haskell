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
import Control.Lens ((&), (.~), ix, (^?!))

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

maxlev = 12               -- max level of an input line + 1

-- type TpConfmat = IOUArray (Int, Int) Int
type TpAxle = ([[Int]], [[Int]], Int)
type TpCond = ([Int], [Int])
-- type TpAdjmat = IOUArray (Int, Int) Int
-- type TpVertices = (IOUArray (Int, Int) Int, Int)
-- type TpQuestion = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
-- type TpEdgelist = IOUArray (Int, Int, Int) Int
type TpPosout = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]], [Int])


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
              -- nosymを書き換える
              (cond2, (low2, upp2, _)) <- checkCondition2 (nn, mm) (low, upp, lev) n m
              mainLoop posout cond2 deg nosym (low2, upp2, lev+1) (tail tactics)
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
checkCondition1 (nn, mm) deg aA@(low, upp, lev) n m nosym = return nosym
  -- check condition and compatibility with A
  -- if (n < 1 || n > 5 * deg)
  --   Error("Invalid vertex in condition", lineno);
  -- if (m < -8 || m > 9 || (m > -5 && m < 6))
  --   Error("Invalid condition", lineno);
  -- let j = (n - 1) `div` deg
  -- let i = (n - 1) `mod` deg + 1
  -- if (n > 2 * deg && (A->low[i] != A->upp[i] || A->low[i] < j + 4))
  --  Error("Condition not compatible with A", lineno);

{-
  -- remember symmetry unless contains a fan vertex
  good <- newIORef =<< (return True :: IO Bool)
  forM_ [0, 1 .. lev] $ \i -> do
    condIN <- readArray nn i
    if condIN > 2 * deg || condIN < 1
      then
        writeIORef good False
      else
        putStr ""
  if good
    then -- remember symmetry
      --if (*pnosym >= MAXSYM)
      --  Error("Too many symmetries", lineno);
      --if (print >= PRTBAS) {
      --  (void) printf("Adding symmetry:");
      T = &sym[(*pnosym)++];
      T->number = lineno;
      T->value = 1;
      T->nolines = lev + 1;
      for (i = 0; i <= lev; ++i) {
        T->pos[i] = cond[i].n;
        if (cond[i].m > 0) {
          T->low[i] = cond[i].m;
          T->upp[i] = INFTY;
        } else {
          T->low[i] = 5;
          T->upp[i] = -cond[i].m;
        }
        --if (print >= PRTBAS)
        --  (void) printf(" (%d,%d,%d)", T->pos[i], T->low[i], T->upp[i]);
      }
    else
      putStr ""
    -- if (print >= PRTBAS) {
    --(void) printf("Symmetry not added\n");
    --(void) fflush(stdout);
-}


checkCondition2 :: TpCond -> TpAxle -> Int -> Int -> IO (TpCond, TpAxle)
checkCondition2 (nn, mm) aA@(low, upp, lev) n m = do
  let low2 = low & ix (lev+1) .~ (low ^?! ix lev)
  let upp2 = upp & ix (lev+1) .~ (upp ^?! ix lev)
  let aLowN = (low2 ^?! ix lev) ^?! ix n
  let aUppN = (upp2 ^?! ix lev) ^?! ix n
  if m > 0
    then -- new lower bound
      if aLowN >= m || m > aUppN
        then
          error "Invalid lower bound in condition"
        else do
          let upp3 = upp2 & (ix lev     <<< ix n) .~ (m-1)
          let low3 = low2 & (ix (lev+1) <<< ix n) .~ m
          let nnnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0
          let mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          return ((nnnn, mmm), (low3, upp3, lev))
    else -- new upper bound
      if aLowN > -m || -m >= aUppN
        then
          error "Invalid upper bound in condition"
        else do
          let low3 = low2 & (ix lev     <<< ix n) .~ (1-m)
          let upp3 = upp2 & (ix (lev+1) <<< ix n) .~ (-m)
          let nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0
          let mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          return ((nnn, mmm), (low3, upp3, lev))



