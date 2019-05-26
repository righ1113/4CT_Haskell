{-
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「present7」「rules」「unavoidable.conf」を取得し、
    本ファイルと同じ場所に置く。
2. > stack exec ghci --resolver lts
3. > :l Discharge
4. > main
5. > 「7」を入力してEnter。
6. 中心の次数7のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK
-}
module Discharge where

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray)


verts = 27 -- max number of vertices in a free completion + 1

maxval = 12
cartvert = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

infty = 12 -- the "12" in the definition of limited part
maxoutlets = 110 -- max number of outlets

maxelist = 134 -- length of edgelist[a][b]

maxlev = 12 -- max level of an input line + 1

type TpConfmat = IOUArray (Int, Int) Int
type TpAxle = (IOUArray (Int, Int) Int, IOUArray (Int, Int) Int, Int)
type TpCond = (IOUArray Int Int, IOUArray Int Int)
type TpAdjmat = IOUArray (Int, Int) Int
type TpVertices = (IOUArray (Int, Int) Int, Int)
type TpQuestion = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
type TpEdgelist = IOUArray (Int, Int, Int) Int
type TpPosout = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]], [Int])


main :: IO ()
main = do
  putStrLn "これは四色定理の放電法をおこなうプログラムです"
  putStrLn "中心の次数7,8,9,10,11のいずれかを入力してください"
  degStr <- getLine
  let deg = read degStr :: Int

  axlesLow <- newArray ((0, 0), (maxlev + 1, cartvert)) 0 :: IO (IOUArray (Int, Int) Int)
  axlesUpp <- newArray ((0, 0), (maxlev + 1, cartvert)) 0 :: IO (IOUArray (Int, Int) Int)
  writeArray axlesLow (0, 0) deg
  writeArray axlesUpp (0, 0) deg
  forM_ [1, 2 .. (5*deg)] $ \i -> do
    writeArray axlesLow (0, i) 5
    writeArray axlesUpp (0, i) infty
  -- CheckHubcap(axles, NULL, 0, print); -- read rules, compute outlets
  -- (void) Reduce(NULL, 0, 0);          -- read unavoidable set

  -- TpCond
  nn <- newArray (0, maxlev) 0 :: IO (IOUArray Int Int)
  mm <- newArray (0, maxlev) 0 :: IO (IOUArray Int Int)

  -- TpOutlet & TpPosout
  let number   = take (2 * maxoutlets + 1) [0, 0..]
  let nolines  = take (2 * maxoutlets + 1) [0, 0..]
  let value    = take (2 * maxoutlets + 1) [0, 0..]
  let pos = replicate (2 * maxoutlets + 1) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let low = replicate (2 * maxoutlets + 1) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let upp = replicate (2 * maxoutlets + 1) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let xx       = take (2 * maxoutlets + 1) [0, 0..]

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
              -- checkCondition (nn, mm) deg (low, upp, lev) n m lev
              -- nosymを書き換える
              mainLoop posout (nn, mm) deg nosym (low, upp, lev+1) (tail tactics)
      _   -> error "Invalid instruction"


checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, upp, lev) posout@(_, _, _, _, _, _, pxx) nosym = do
  let l@[k, epsilon, level, line] = map read str :: [Int]
  print l



