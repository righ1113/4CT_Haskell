module ReLibAngles where

import CoLibCConst
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


findangles :: TpConfmat -> TpEdgeno -> (TpAngle, TpAngle, TpAngle, [Int])
findangles gConf edgeno = (angle3, diffangle3, sameangle0, contract3) where
  contract0  = replicate (edges + 1) 0
  angle0     = replicate edges $ replicate 5 0
  diffangle0 = replicate edges $ replicate 5 0
  sameangle0 = replicate edges $ replicate 5 0

  contract1 = contract0  & ix 0     .~ head (gConf !! (1 + 1)) -- number of edges in contract
  contract2 = contract1  & ix edges .~ (gConf !! (0 + 1)) !! 3
  contract3 = findanglesSub1 gConf edgeno contract2 1

  edge       = 3 * head (gConf !! (0 + 1))  - 3 - (gConf !! (0 + 1)) !! 1
  diffangle1 = diffangle0  & (ix 0 <<< ix 0) .~ head (gConf !! (0 + 1))
  diffangle2 = diffangle1  & (ix 0 <<< ix 1) .~ (gConf !! (0 + 1)) !! 1
  diffangle3 = diffangle2  & (ix 0 <<< ix 2) .~ edge

  angle1 = angle0  & (ix 0 <<< ix 0) .~ head (head diffangle3)
  angle2 = angle1  & (ix 0 <<< ix 1) .~ head diffangle3 !! 1
  angle3 = angle2  & (ix 0 <<< ix 2) .~ head diffangle3 !! 2

findanglesSub1 :: TpConfmat -> TpEdgeno -> [Int] -> Int -> [Int]
findanglesSub1 gConf edgeno contract i
  | i > head contract      = contract
  | (edgeno !! u) !! v < 1 = error "***  ERROR: CONTRACT CONTAINS NON-EDGE  ***"
  | otherwise              = findanglesSub1 gConf edgeno contract2 (i + 1) where
    u         = (gConf !! 2) !! (2 * i - 1)
    v         = (gConf !! 2) !! (2 * i)
    contract2 = contract  & ix ((edgeno !! u) !! v) .~ 1



