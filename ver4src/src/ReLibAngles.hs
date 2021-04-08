module ReLibAngles where

import CoLibCConst ( edges, TpAngle, TpConfmat, TpEdgeno )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


findangles :: TpConfmat -> TpEdgeno -> (TpAngle, TpAngle, TpAngle, [Int])
findangles gConf edgeno = (angle4, diffangle4, sameangle4, contract5) where
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

  (angle4, diffangle4, sameangle4, contract4) = findanglesSub2 gConf edgeno (angle3, diffangle3, sameangle0, contract3)
  contract5                                   = findanglesSub3 gConf contract4


findanglesSub1 :: TpConfmat -> TpEdgeno -> [Int] -> Int -> [Int]
findanglesSub1 gConf edgeno contract i
  | i > head contract      = contract
  | (edgeno !! u) !! v < 1 = error "***  ERROR: CONTRACT CONTAINS NON-EDGE  ***"
  | otherwise              = findanglesSub1 gConf edgeno contract2 (i + 1) where
      u         = (gConf !! 2) !! (2 * i - 1)
      v         = (gConf !! 2) !! (2 * i)
      contract2 = contract  & ix ((edgeno !! u) !! v) .~ 1


findanglesSub2 :: TpConfmat -> TpEdgeno -> (TpAngle, TpAngle, TpAngle, [Int]) -> (TpAngle, TpAngle, TpAngle, [Int])
findanglesSub2 gConf edgeno (angle, diffangle, sameangle, contract) = (angle, diffangle, sameangle, contract)


-- check assert
findanglesSub3 :: TpConfmat -> [Int] -> [Int]
findanglesSub3 gConf contract = contract



