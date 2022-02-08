{-
◆author: righ1113
◆動かし方
1. $ stack run reduce-exe
1. $ stack ghci --main-is ver4src:exe:reduce-exe
2. > main
2. > :l app/Reduce
-}
module Main where

import CoLibCConst     ( readFileGoodConfsR, TpConfmat, power, simatchnumber ) 
import ReLibStrip      ( strip, getEdgeNo )
import ReLibAngles     ( findangles2 )
import ReLibFindlive   ( findlive )
import ReLibUpdateLive ( updateLive )


main :: IO ()
main = do
  putStrLn "これは四色定理の可約性を調べるプログラムです"
  gConfs <- readFileGoodConfsR
  mainLoop gConfs
  -- putStrLn "633個の好配置は全て、Ｄ可約 or Ｃ可約です"
  putStrLn "プログラムは正常終了しました"


mainLoop :: [TpConfmat] -> IO ()
mainLoop gConfs
  | null gConfs = return ()
  | otherwise   = do

    -- 1. strip()
    let gConf  = head gConfs
        vertex = head $ gConf !! 1
        ring   = gConf !! 1 !! 1                   -- ring-size
        edgeno = strip ring gConf
    -- putStrLn "edgeno:"
        edgeNo = getEdgeNo vertex ring gConf
    -- print ret

    -- 2. findangles()
    {- "findangles" fills in the arrays "angle","diffangle","sameangle" and
        "contract" from the input "graph". "angle" will be used to compute
        which colourings of the ring edges extend to the configuration; the
        others will not be used unless a contract is specified, and if so
        they will be used in "checkcontract" below to verify that the
        contract is correct. -}
    -- let (angle, diffangle, sameangle, contract) = findangles gConf edgeno
    let (angle, diffangle, sameangle, contract) = findangles2 (gConf, edgeno)
    --print contract2

    -- 3. findlive()
        ncodes = (power !! ring + 1) `div` 2       -- number of codes of colorings of R
        bigno  = (power !! (ring + 1) - 1) `div` 2 -- needed in "inlive"
        live0  = replicate ncodes 1
    (nlive1, live1) <- findlive ring bigno live0 ncodes angle power (gConf !! 1 !! 2)

    -- 4. updatelive()
        --real   = replicate (simatchnumber !! maxring `div` 8 + 2) 255
    let nchar  = (simatchnumber !! ring) `div` 8 + 1
    -- computes {\cal M}_{i+1} from {\cal M}_i, updates the bits of "real"
    (nlive2, live2) <- updateLive ring nchar ncodes (nlive1, live1)
    -- computes {\cal C}_{i+1} from {\cal C}_i, updates "live"

    -- 5. checkContract()
    {- This verifies that the set claimed to be a contract for the
        configuration really is. -}
    --checkContract live2 nlive2 diffangle sameangle contract

    -- 6 . recursion
    -- mainLoop $ tail gConfs
    mainLoop []



