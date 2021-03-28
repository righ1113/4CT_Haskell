module ReLibAngles where

import CoLibCConst
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


findangles :: TpConfmat -> TpEdgeno -> (TpAngle, TpAngle, TpAngle, [Int])
findangles gConf edgeno = (angle3, diffangle3, sameangle0, contract0) where
  contract0  = replicate (edges + 1) 0
  angle0     = replicate edges $ replicate 5 0
  diffangle0 = replicate edges $ replicate 5 0
  sameangle0 = replicate edges $ replicate 5 0

  {- @contract[0] = g_conf[1 + 1][0] -- number of edges in contract
  @contract[Const::EDGES] = g_conf[0 + 1][3]
  @contract[0].times do |ii|
    i = ii + 1
    u = g_conf[2][2 * i - 1]
    v = g_conf[2][2 * i]
    Assert.assert_equal (edgeno[u][v] >= 1), true, '***  ERROR: CONTRACT CONTAINS NON-EDGE  ***'
    @contract[edgeno[u][v]] = 1
  end -}

  edge       = 3 * head (gConf !! (0 + 1))  - 3 - (gConf !! (0 + 1)) !! 1
  diffangle1 = diffangle0  & (ix 0 <<< ix 0) .~ head (gConf !! (0 + 1))
  diffangle2 = diffangle1  & (ix 0 <<< ix 1) .~ (gConf !! (0 + 1)) !! 1
  diffangle3 = diffangle2  & (ix 0 <<< ix 2) .~ edge

  angle1 = angle0  & (ix 0 <<< ix 0) .~ head (head diffangle3)
  angle2 = angle1  & (ix 0 <<< ix 1) .~ head diffangle3 !! 1
  angle3 = angle2  & (ix 0 <<< ix 2) .~ head diffangle3 !! 2



