module ReLibAngles where

import CoLibCConst   ( edges, TpAngle, TpConfmat, TpEdgeno )
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

  (angle4, diffangle4, sameangle4, contract4) = findanglesSub2 gConf edgeno 1 (angle3, diffangle3, sameangle0, contract3)
  contract5                                   = findanglesSub3 gConf contract4


findanglesSub1 :: TpConfmat -> TpEdgeno -> [Int] -> Int -> [Int]
findanglesSub1 gConf edgeno contract i
  | i > head contract      = contract
  | (edgeno !! u) !! v < 1 = error "***  ERROR: CONTRACT CONTAINS NON-EDGE  ***"
  | otherwise              = findanglesSub1 gConf edgeno contract2 (i + 1) where
      u         = (gConf !! 2) !! (2 * i - 1)
      v         = (gConf !! 2) !! (2 * i)
      contract2 = contract  & ix ((edgeno !! u) !! v) .~ 1


findanglesSub2 :: TpConfmat -> TpEdgeno -> Int -> (TpAngle, TpAngle, TpAngle, [Int]) -> (TpAngle, TpAngle, TpAngle, [Int])
findanglesSub2 gConf edgeno v (angle, diffangle, sameangle, contract)
  | v > head (gConf !! (0 + 1)) = (angle, diffangle, sameangle, contract)
  | otherwise                   = findanglesSub2 gConf edgeno (v + 1) (angle2, diffangle2, sameangle2, contract2) where
      (angle2, diffangle2, sameangle2, contract2) = findanglesSub2Sub gConf edgeno v 1 (angle, diffangle, sameangle, contract)


findanglesSub2Sub :: TpConfmat -> TpEdgeno -> Int -> Int -> (TpAngle, TpAngle, TpAngle, [Int]) -> (TpAngle, TpAngle, TpAngle, [Int])
findanglesSub2Sub gConf edgeno v h (angle, diffangle, sameangle, contract)
  | h > (gConf !! (v + 2)) !! 1 = (angle, diffangle, sameangle, contract)
  | otherwise                   = findanglesSub2Sub gConf edgeno v (h + 1) (angle, diffangle, sameangle, contract)
{-
      g_conf[0 + 1][0].times do |vv|
        v = vv + 1
        g_conf[v + 2][0 + 1].times do |hh|
          h = hh + 1
          next if v <= g_conf[0 + 1][1] && h == g_conf[v + 2][0 + 1]
          break 0 if h >= g_conf[v + 2].length
          i = h < g_conf[v + 2][1] ? h + 1 : 1
          u = g_conf[v + 2][h + 1]
          w = g_conf[v + 2][i + 1]
          a = edgeno[v][w]
          b = edgeno[u][w]
          c = edgeno[u][v]
          # どっちかが0なら通過
          str = '***  ERROR: CONTRACT IS NOT SPARSE  ***'
          Assert.assert_equal (@contract[a].zero? || @contract[b].zero?), true, str
          angles_sub2_sub a, b, c
          angles_sub2_sub b, a, c
        end
      end
-}

-- check assert
findanglesSub3 :: TpConfmat -> [Int] -> [Int]
findanglesSub3 gConf contract = contract



