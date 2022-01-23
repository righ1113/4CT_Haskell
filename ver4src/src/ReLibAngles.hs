module ReLibAngles where

import CoLibCConst   ( edges, mverts, TpAngle, TpConfmat, TpEdgeno, TpAnglePack )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Function ( fix )


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
  | h > (gConf !! (v + 2)) !! 1                                  = (angle, diffangle, sameangle, contract)
  | v <= (gConf !! (0 + 1)) !! 1 && h == (gConf !! (v + 2)) !! 1 = findanglesSub2Sub gConf edgeno v (h + 1) (angle, diffangle, sameangle, contract)
  | h >= length (gConf !! (v + 2))                               = (angle, diffangle, sameangle, contract)
  | contract !! a /= 0 && contract !! b /= 0                     = error "***  ERROR: CONTRACT IS NOT SPARSE  ***"
  | otherwise                                                    = findanglesSub2Sub gConf edgeno v (h + 1) (angle3, diffangle3, sameangle3, contract3) where
      i = if h < (gConf !! (v + 2)) !! 1 then h + 1 else 1
      u = (gConf !! (v + 2)) !! (h + 1)
      w = (gConf !! (v + 2)) !! (i + 1)
      a = (edgeno !! v) !! w
      b = (edgeno !! u) !! w
      c = (edgeno !! u) !! v
      (angle2, diffangle2, sameangle2, contract2) = findanglesSub2SubSub a b c (angle,  diffangle,  sameangle,  contract)
      (angle3, diffangle3, sameangle3, contract3) = findanglesSub2SubSub b a c (angle2, diffangle2, sameangle2, contract2)


findanglesSub2SubSub :: Int -> Int -> Int -> (TpAngle, TpAngle, TpAngle, [Int]) -> (TpAngle, TpAngle, TpAngle, [Int])
findanglesSub2SubSub x y c (angle, diffangle, sameangle, contract)
  | x <= c             = (angle,  diffangle,  sameangle,  contract)
  | contract !! y == 0 = (angle3, diffangle3, sameangle,  contract)
  | otherwise          = (angle3, diffangle3, sameangle3, contract) where
      angle2 = if head (angle !! c) >= 4 then angle else angle & (ix c <<< ix 0) .~ head (angle !! c) + 1
      d      = if head (angle !! c) >= 4 then 4     else                            head (angle !! c) + 1
      angle3 = angle2 & (ix c <<< ix d) .~ x
      diffangle2
        | contract !! x == 0 && contract !! y == 0 && contract !! c == 0
            = if head (diffangle !! c) >= 4 then diffangle else angle & (ix c <<< ix 0) .~ head (diffangle !! c) + 1
        | otherwise = diffangle
      e
        | contract !! x == 0 && contract !! y == 0 && contract !! c == 0
            = if head (diffangle !! c) >= 4 then 4         else                            head (diffangle !! c) + 1
        | otherwise = 0
      diffangle3
        = if contract !! x == 0 && contract !! y == 0 && contract !! c == 0 then diffangle2 & (ix c <<< ix e) .~ x else diffangle
      sameangle2 = if head (sameangle !! c) >= 4 then sameangle else sameangle & (ix c <<< ix 0) .~ head (sameangle !! c) + 1
      f          = if head (sameangle !! c) >= 4 then 4         else                                head (sameangle !! c) + 1
      sameangle3 = sameangle2 & (ix c <<< ix f) .~ x


-- check assert
findanglesSub3 :: TpConfmat -> [Int] -> [Int]
findanglesSub3 gConf contract
  | head contract < 4 = contract -- checking that there is a triad
  | findanglesSub3Sub gConf neighbour (((gConf !! (0 + 1)) !! 1) + 1) = contract
  | otherwise = error "***  ERROR: CONTRACT HAS NO TRIAD  ***" where
      neighbour = replicate mverts False


findanglesSub3Sub :: TpConfmat -> [Bool] -> Int -> Bool
findanglesSub3Sub gConf neighbour v
  | v > head (gConf !! (0 + 1)) = False
  | otherwise = True where --findanglesSub3Sub gConf neighbour (v + 1)
      _a = findanglesSub3SubSub gConf neighbour v 0 1


findanglesSub3SubSub :: TpConfmat -> [Bool] -> Int -> Int -> Int -> Int
findanglesSub3SubSub gConf neighbour v a i
  | i > (gConf !! (v + 2)) !! (0 + 1) = a
  | a1 < 3                            = findanglesSub3SubSub gConf neighbour v a1 (i + 1)
  | head (gConf !! (v + 2)) >= 6      = a1 -- ???
  | retFlg                            = a1
  | otherwise                         = findanglesSub3SubSub gConf neighbour v a1 (i + 1) where
      u          = (gConf !! (v + 2)) !! (i + 1)
      a1         = findanglesSub3SubSubSub1 gConf u a 0
      neighbour2 = findanglesSub3SubSubSub2 gConf neighbour  1
      neighbour3 = findanglesSub3SubSubSub3 gConf neighbour2 1 v
      retFlg     = findanglesSub3SubSubSub4 gConf neighbour3 1


findanglesSub3SubSubSub1 :: TpConfmat -> Int -> Int -> Int -> Int
findanglesSub3SubSubSub1 gConf u a j
  | j > 8     = a
  | otherwise = findanglesSub3SubSubSub1 gConf u a1 (j + 1) where
      a1 = if u == (gConf !! 2) !! j then a + 1 else a


findanglesSub3SubSubSub2 :: TpConfmat -> [Bool] -> Int -> [Bool]
findanglesSub3SubSubSub2 gConf neighbour u
  | u > head (gConf !! (0 + 1)) = neighbour
  | otherwise                   = findanglesSub3SubSubSub2 gConf neighbour2 (u + 1) where
      neighbour2 = neighbour & ix u .~ False


findanglesSub3SubSubSub3 :: TpConfmat -> [Bool] -> Int -> Int -> [Bool]
findanglesSub3SubSubSub3 gConf neighbour u v
  | u > (gConf !! (v + 2)) !! (0 + 1) = neighbour
  | otherwise                         = findanglesSub3SubSubSub3 gConf neighbour2 (u + 1) v where
      neighbour2 = neighbour & ix ((gConf !! (v + 2)) !! u) .~ True


findanglesSub3SubSubSub4 :: TpConfmat -> [Bool] -> Int -> Bool
findanglesSub3SubSubSub4 gConf neighbour j
  | j > 8                                  = False
  | neighbour !! ((gConf !! (0 + 2)) !! j) = True -- return
  | otherwise                              = findanglesSub3SubSubSub4 gConf neighbour (j + 1)


findangles2 :: (TpConfmat, TpEdgeno) -> (TpAngle, TpAngle, TpAngle, [Int])
findangles2 = findangleSub3 . findangleSub2 1 . findangleSub1 1 . findangleSub0


-- ======== findangleSub0 ========
findangleSub0 :: (TpConfmat, TpEdgeno) -> TpAnglePack
findangleSub0 (gConf, edgeno) = (gConf, edgeno, angle3, diffangle3, sameangle0, contract2) where
  contract0  = replicate (edges + 1) 0
  angle0     = replicate edges $ replicate 5 0
  diffangle0 = replicate edges $ replicate 5 0
  sameangle0 = replicate edges $ replicate 5 0

  contract1 = contract0  & ix 0     .~ head (gConf !! 2) -- number of edges in contract
  contract2 = contract1  & ix edges .~ (gConf !! 1) !! 3

  edge       = 3 * head (gConf !! 1)  - 3 - (gConf !! 1) !! 1
  diffangle1 = diffangle0  & (ix 0 <<< ix 0) .~ head (gConf !! 1)
  diffangle2 = diffangle1  & (ix 0 <<< ix 1) .~ (gConf !! 1) !! 1
  diffangle3 = diffangle2  & (ix 0 <<< ix 2) .~ edge
  angle1     = angle0      & (ix 0 <<< ix 0) .~ head (head diffangle3)
  angle2     = angle1      & (ix 0 <<< ix 1) .~ head diffangle3 !! 1
  angle3     = angle2      & (ix 0 <<< ix 2) .~ head diffangle3 !! 2


-- ======== findangleSub1 ========
findangleSub1 :: Int -> TpAnglePack -> TpAnglePack
findangleSub1 i pack@(gConf, edgeno, angle, diffangle, sameangle, contract)
  | i > head contract = pack
  | otherwise         = findangleSub1 (i + 1) (gConf, edgeno, angle, diffangle, sameangle, contract2) where
      u = (gConf !! 2) !! (2 * i - 1)
      v = (gConf !! 2) !!  2 * i
      contract2 = if (edgeno !! u) !! v < 1 then error "***  ERROR: CONTRACT CONTAINS NON-EDGE  ***" else contract & ix ((edgeno !! u) !! v) .~ 1



-- ======== findangleSub2 ========
findangleSub2 :: Int -> TpAnglePack -> TpAnglePack
findangleSub2 v pack@(gConf, edgeno, angle, diffangle, sameangle, contract)
  | v > head (gConf !! 1) = pack
  | otherwise             = findangleSub2 (v + 1) nextPack where
      nextPack = flip fix (pack, 1) $ \loop (pack2@(gc, ed, an, di, sa, co), h) -> case () of
                  _ | h > (gc !! (v + 2)) !! 1                         ->       pack2         -- end
                    | v <= (gc !! 1) !! 1 && h == (gc !! (v + 2)) !! 1 -> loop (pack2, h + 1) -- next
                    | h >= length (gc !! (v + 2))                      ->       pack2         -- break
                    | otherwise                                        -> loop (pack3, h + 1) where
                        i = if h < (gc !! (v + 2)) !! 1 then h + 1 else 1
                        u = (gc !! (v + 2)) !! (h + 1)
                        w = (gc !! (v + 2)) !! (i + 1)
                        a = ed !! v !! w
                        b = ed !! u !! w
                        -- どっちかが0なら通過
                        c = if 0 /= co !! a && 0 /= co !! b then error "***  ERROR: CONTRACT IS NOT SPARSE  ***" else edgeno !! u !! v
                        pack3 = (setAngle b a c . setAngle a b c) pack2 


setAngle :: Int -> Int -> Int -> TpAnglePack -> TpAnglePack
setAngle x y c pack@(gc, ed, an, di, sa, co) = undefined

{-
    def angles_sub2_sub(xxx, yyy, ccc)
      x, y, c = xxx, yyy, ccc
      return unless x > c
      d  = @angle[c][0] >= 4 ? 4 : @angle[c][0] += 1
      @angle[c][d] = x
      if @contract[x].zero? && @contract[y].zero? && @contract[c].zero?
        e = @diffangle[c][0] >= 4 ? 4 : @diffangle[c][0] += 1
        @diffangle[c][e] = x
      end
      return if @contract[y].zero?
      e = @sameangle[c][0] >= 4 ? 4 : @sameangle[c][0] += 1
      @sameangle[c][e] = x
    end
-}
-- ======== findangleSub3 ========
findangleSub3 = undefined



