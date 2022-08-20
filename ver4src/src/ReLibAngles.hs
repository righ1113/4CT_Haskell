module ReLibAngles where

import CoLibCConst   ( edges, TpAngle, TpConfmat, TpEdgeNo, TpAnglePack )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Function ( fix )


findAngle :: (TpConfmat, TpEdgeNo) -> (TpAngle, TpAngle, TpAngle, [Int])
findAngle = findAngleSub3 . findAngleSub2 1 . findAngleSub1 1 . findAngleSub0


-- ======== findangleSub0 ========
findAngleSub0 :: (TpConfmat, TpEdgeNo) -> TpAnglePack
findAngleSub0 (gConf, edgeNo) = (gConf, edgeNo, angle3, diffAngle3, sameAngle0, contract2) where
  contract0  = replicate (edges + 1) 0
  angle0     = replicate edges $ replicate 5 0
  diffAngle0 = replicate edges $ replicate 5 0
  sameAngle0 = replicate edges $ replicate 5 0

  contract1 = contract0  & ix 0     .~ head (gConf !! 2) -- number of edges in contract
  contract2 = contract1  & ix edges .~ (gConf !! 1) !! 3

  edge       = 3 * head (gConf !! 1)  - 3 - (gConf !! 1) !! 1
  diffAngle1 = diffAngle0  & (ix 0 <<< ix 0) .~ head (gConf !! 1)
  diffAngle2 = diffAngle1  & (ix 0 <<< ix 1) .~ (gConf !! 1) !! 1
  diffAngle3 = diffAngle2  & (ix 0 <<< ix 2) .~ edge
  angle1     = angle0      & (ix 0 <<< ix 0) .~ head (head diffAngle3)
  angle2     = angle1      & (ix 0 <<< ix 1) .~ head diffAngle3 !! 1
  angle3     = angle2      & (ix 0 <<< ix 2) .~ head diffAngle3 !! 2


-- ======== findangleSub1 ========
findAngleSub1 :: Int -> TpAnglePack -> TpAnglePack
findAngleSub1 i pack@(gConf, edgeNo, angle, diffAngle, sameAngle, contract)
  | i > head contract = pack
  | otherwise         = findAngleSub1 (i + 1) (gConf, edgeNo, angle, diffAngle, sameAngle, contract2) where
      u = (gConf !! 2) !! (2 * i - 1)
      v = (gConf !! 2) !! (2 * i)
      contract2 = if edgeNo !! u !! v < 1 then error ("***  ERROR: CONTRACT CONTAINS NON-EDGE  *** " ++ show u ++ " " ++ show v) else contract & ix (edgeNo !! u !! v) .~ 1



-- ======== findangleSub2 ========
findAngleSub2 :: Int -> TpAnglePack -> TpAnglePack
findAngleSub2 v pack@(gConf, _, _, _, _, _)
  | v > head (gConf !! 1) = pack
  | otherwise             = findAngleSub2 (v + 1) nextPack where
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
                        c = if 0 /= co !! a && 0 /= co !! b then error "***  ERROR: CONTRACT IS NOT SPARSE  ***" else ed !! u !! v
                        pack3 = (setAngle b a c . setAngle a b c) pack2 


setAngle :: Int -> Int -> Int -> TpAnglePack -> TpAnglePack
setAngle x y c pack@(gc, ed, an, di, sa, co) =
  let
    an2   = an & (ix c <<< ix 0) .~ head (an !! c) + 1
    d     = head (an2 !! c)
    an3   = an2 & (ix c <<< ix d) .~ x
    bool1 = 0 == co !! x && 0 == co !! y && 0 == co !! c
    di2   = di & (ix c <<< ix 0) .~ head (di !! c) + 1
    e     = head (di2 !! c)
    di3   = di2 & (ix c <<< ix e) .~ x
    bool2 = 0 == co !! y
    sa2   = sa2 & (ix c <<< ix 0) .~ head (sa !! c) + 1
    f     = head (sa2 !! c)
    sa3   = sa2 & (ix c <<< ix f) .~ x
  in case () of
    _ | x <= c    -> pack
      | bool1     -> case () of
                      _ | bool2     -> (gc, ed, an3, di3, sa,  co)
                        | otherwise -> (gc, ed, an3, di3, sa3, co)
      | otherwise -> case () of
                      _ | bool2     -> (gc, ed, an3, di,  sa,  co)
                        | otherwise -> (gc, ed, an3, di,  sa3, co)


-- ======== findangleSub3 ========
-- check assert
findAngleSub3 :: TpAnglePack -> (TpAngle, TpAngle, TpAngle, [Int])
findAngleSub3 (gc, ed, an, di, sa, co) = (an, di, sa, co)



