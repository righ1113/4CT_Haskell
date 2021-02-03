{-
◆author: righ1113
◆動かし方
1. $ stack run discharge-exe
2. > 「07」を入力してEnter。
3. 中心の次数 07 のグラフは、電荷が負になるか、近くに好配置があらわれるかです
    プログラムは正常終了しました
    が表示されたら OK
-}
module Main where

import CoLibCConst
    ( TpReducePack,
      TpPosout,
      TpGoodConf,
      TpCond,
      TpAxle,
      cartvert,
      infty,
      maxoutlets,
      maxelist,
      maxlev )
import DiLibCondition            ( checkCondition1, checkCondition2 )
import DiLibHubcap               ( checkHubcap )
import DiLibReduce               ( reduce )
import DiLibSymmetry             ( delSym, checkSymmetry )
import Control.Lens              ( (&), (.~), Ixed(ix) )
import Control.Monad.IO.Class    ( liftIO )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.Trans.RWS   ( RWST(..), ask, get, put )
import Data.Maybe                ( isNothing )


main :: IO ()
main = do

  putStrLn "これは四色定理の放電法をおこなうプログラムです。"
  putStrLn "中心の次数 07, 08, 09, 10, 11 のいずれかを入力してください。"

  -- deg
  degStr <- getLine
  let deg = read degStr :: Int

  -- TpAxle
  let axles0 = replicate maxlev $ replicate cartvert 0
  let axLow  = take cartvert ([deg] ++ replicate (5 * deg) 5     ++ repeat 0) : axles0
  let axUpp  = take cartvert ([deg] ++ replicate (5 * deg) infty ++ repeat 0) : axles0

  -- TpCond
  let nn = replicate maxlev 0
  let mm = replicate maxlev 0

  -- TpPosout
  rulesStr   <- readFile $ "4ctdata/DiRules" ++ degStr ++ ".txt"
  let rules   = read rulesStr :: TpPosout -- read rules, compute outlets
  let posoutX = replicate (2 * maxoutlets) 0

  -- sym
  let symNum = replicate (2 * maxoutlets) 0
  let symNol = replicate (2 * maxoutlets) 0
  let symVal = replicate (2 * maxoutlets) 0
  let symPos = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let symLow = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let symUpp = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  -- TpReducePack
  let aSLow    = replicate (maxlev + 1) $ replicate cartvert 0
  let aSUpp    = replicate (maxlev + 1) $ replicate cartvert 0
  --let bLow     = replicate cartvert 0
  --let bUpp     = replicate cartvert 0
  let adjmat   = replicate cartvert $ replicate cartvert 0
  let edgelist = replicate 12 $ replicate 9 $ replicate maxelist 0
  let used     = replicate cartvert False
  let image    = replicate cartvert 0
  gConfsStr   <- readFile "4ctdata/DiGoodConfs.txt"
  let gConfs   = read gConfsStr :: [TpGoodConf] -- read GoodConfs set

  -- Tactics
  inStr <- readFile $ "4ctdata/DiTactics" ++ degStr ++ ".txt"

  -- mainLoop
  (ret, _, _)
    <- runRWST (mainLoop  (nn, mm)
                          (symNum, symNol, symVal, symPos, symLow, symUpp)
                          0
                          (axLow, axUpp, 0)
                          (tail (map words (lines inStr)))
                          2 )
                (gConfs, rules, deg)                                          -- read
                (((aSLow, aSUpp, 0), used, image, adjmat, edgelist), posoutX) -- state

  -- final check
  if ret == "Q.E.D." then
    putStrLn $ "中心の次数 " ++ degStr ++ " のグラフは、電荷が負になるか、近くに好配置があらわれるかです。"
  else
    putStrLn "失敗です。"

  putStrLn "プログラムは正常終了しました。"

mainLoop :: TpCond -> TpPosout -> Int -> TpAxle -> [[String]] -> Int
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
mainLoop (nn, mm) sym@(_, symNol, _, _, _, _) nosym ax@(axLow, axUpp, axLev) tactics lineno
  | axLev >= maxlev = error "More than %d levels"
  | axLev < 0       = return $ head $ head tactics
  | otherwise       = let nowTac = head tactics in
      case nowTac !! 1 of
        "C" -> do
          liftIO . putStrLn $ "Condition  " ++ show nowTac
          (_, _, deg)              <- ask
          let n                     = read (head tactics !! 2) :: Int
              m                     = read (head tactics !! 3) :: Int
              (low2, upp2, _axLev2) = checkCondition1 (nn, mm) ax n m
              (sym2, nosym2)        = checkCondition2 (nn, mm) ax deg sym nosym lineno
              nn2                   = (nn & ix axLev .~ n) & ix (axLev + 1) .~ 0
              mm2                   = (mm & ix axLev .~ m) & ix (axLev + 1) .~ 0
          mainLoop (nn2, mm2) sym2 nosym2 (low2, upp2, axLev + 1) (tail tactics) (lineno + 1)
        "H" -> do
          liftIO . putStrLn $ "Hubcap  " ++ show nowTac
          checkHubcap (tail (tail (head tactics))) ax
          let nosym2 = delSym nosym symNol axLev
          mainLoop (nn, mm) sym nosym2 (axLow, axUpp, axLev - 1) (tail tactics) (lineno + 1)
        "R" -> do
          liftIO . putStrLn $ "Reduce  " ++ show nowTac
          (((aSLow, aSUpp, aSLev), used, image, adjmat, elist), poX) <- get
          put (((aSLow & ix 0 .~ axLow !! axLev, aSUpp & ix 0 .~ axUpp !! axLev, aSLev), used, image, adjmat, elist), poX)
          ret <- (runMaybeT . reduce) 1
          if isNothing ret then
            error "Reducibility failed"
          else do
            let nosym2 = delSym nosym symNol axLev
            mainLoop (nn, mm) sym nosym2 (axLow, axUpp, axLev - 1) (tail tactics) (lineno + 1)
        "S" -> do
          liftIO . putStrLn $ "Symmetry  " ++ show nowTac
          (_, _, deg) <- ask
          liftIO $ checkSymmetry (tail (tail (head tactics))) ax sym nosym deg
          let _nosym2 = delSym nosym symNol axLev
          mainLoop (nn, mm) sym _nosym2 (axLow, axUpp, axLev - 1) (tail tactics) (lineno + 1)
          -- return "Q.E.D."
        _   -> error "Invalid instruction"



