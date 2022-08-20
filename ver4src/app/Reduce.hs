{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-
◆author: righ1113
◆動かし方
1. $ stack run reduce-exe
1. $ stack ghci --main-is ver4src:exe:reduce-exe
2. > main
2. > :l app/Reduce
-}
module Main where

import CoLibCConst     ( readFileGoodConfsR, TpConfmat, power, siMatchNumber ) 
import ReLibStrip      ( getEdgeNo )
import ReLibAngles     ( findAngle )
import ReLibFindlive   ( findLive )
import ReLibUpdateLive ( updateLive )
import ReLibCRedu      ( checkCReduce )


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"
  gConfs <- readFileGoodConfsR
  mainLoop 0 gConfs
  -- putStrLn "633個の好配置は全て、Ｄ可約 or Ｃ可約です"
  putStrLn "プログラムは正常終了しました"


mainLoop :: Int -> [TpConfmat] -> IO ()
mainLoop cnt gConfs
  | null gConfs = return ()
  | otherwise   = do

  print cnt
  -- 1. getEdgeNo()
  let
    gConf  = gConfs !! 3
    vertex = gConf !! 1 !! 0
    ring   = gConf !! 1 !! 1                   -- ring-size
    -- edgeno = strip ring gConf
    edgeNo = getEdgeNo vertex ring gConf

  -- 2. findangles()
  {- "findangles" fills in the arrays "angle","diffangle","sameangle" and
    "contract" from the input "graph". "angle" will be used to compute
    which colourings of the ring edges extend to the configuration; the
    others will not be used unless a contract is specified, and if so
    they will be used in "checkcontract" below to verify that the
    contract is correct. -}
  -- let (angle, diffangle, sameangle, contract) = findangles gConf edgeno
  let
    (angle, diffangle, sameangle, contract) = findAngle (gConf, edgeNo)
  --print contract2

  -- 3. findlive()
    ncodes = (power !!  ring      + 1) `div` 2 -- number of codes of colorings of R
    bigno  = (power !! (ring + 1) - 1) `div` 2 -- needed in "inlive"
    live0  = replicate ncodes 1
  (nlive1, live1) <- findLive ring bigno live0 ncodes angle power (gConf !! 1 !! 2)

  -- 4. updatelive()
  let nchar  = (siMatchNumber !! ring) `div` 8 + 1
  -- computes {\cal M}_{i+1} from {\cal M}_i, updates the bits of "real"
  (nlive2, live2) <- updateLive (ring, nchar) ncodes (nlive1, live1)
  -- computes {\cal C}_{i+1} from {\cal C}_i, updates "live"

  -- 5. checkContract()
  {- This verifies that the set claimed to be a contract for the
    configuration really is. -}
  if nlive2 == 0 then
    if contract !! 0 == 0 then
      -- D可約 のときは、checkCReduce() を呼ばない
      return True
    else
      error "         ***  ERROR: CONTRACT PROPOSED  ***\n\n"
  else
    checkCReduce ring bigno nlive2 live2 diffangle sameangle contract

  -- 6 . recursion
  if cnt < 30 then
    mainLoop (cnt + 1) $ tail gConfs
  else
    mainLoop (cnt + 1) []



