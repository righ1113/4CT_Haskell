{-
◆author: righ1113
◆動かし方
1. $ stack run reduce-exe
1. $ stack ghci --main-is ver4src:exe:reduce-exe
2. > main
2. > :l app/Reduce
-}
module Main where

import CoLibCConst   ( readFileGoodConfsR, TpConfmat, power ) 
import ReLibStrip    ( strip )
import ReLibAngles   ( findangles )
import ReLibFindlive ( findlive )
import Control.Lens  ( (^?!), Ixed(ix) )


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"
  gConfs <- readFileGoodConfsR
  -- print (((gConfs !! 1) !! 2) !! 1)
  mainLoop gConfs
  -- putStrLn "633個の好配置は全て、Ｄ可約 or Ｃ可約です"
  putStrLn "プログラムは正常終了しました"


mainLoop :: [TpConfmat] -> IO ()
mainLoop gConfs
  | null gConfs = return ()
  | otherwise   = do

    -- 1. strip()
    let gConf  = head gConfs
        edgeno = strip gConf

    -- 2. findangles()
    {- "findangles" fills in the arrays "angle","diffangle","sameangle" and
        "contract" from the input "graph". "angle" will be used to compute
        which colourings of the ring edges extend to the configuration; the
        others will not be used unless a contract is specified, and if so
        they will be used in "checkcontract" below to verify that the
        contract is correct. -}
    let (angle, diffangle, sameangle, contract) = findangles gConf edgeno

    -- 3. findlive()
    let ring   = (gConf ^?! ix 0) ^?! ix 1   -- ring-size
        ncodes = (power !! ring + 1) `div` 2 -- number of codes of colorings of R
        live0  = replicate ncodes 1
        --real0  = replicate (simatchnumber !! maxring `div` 8 + 2) 255
        --nchar  = simatchnumber !! ring `div` 8 + 1
    (nlive1, live1) <- findlive live0 ncodes angle power ((gConf ^?! ix 0) ^?! ix 2)

    -- 4. updatelive()
    -- computes {\cal M}_{i+1} from {\cal M}_i, updates the bits of "real"
    --(nlive2, live2) <- updatelive ring real0 power live0 nchar ncodes 1 -- bug4 live1 nchar ncodes nlive1
    -- computes {\cal C}_{i+1} from {\cal C}_i, updates "live"

    -- 5. checkContract()
    {- This verifies that the set claimed to be a contract for the
        configuration really is. -}
    --checkContract live2 nlive2 diffangle sameangle contract power

    -- 6 . recursion
    -- mainLoop $ tail gConfs
    mainLoop []



