{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module ReLibAngles( findAngle ) where

import CoLibCConst   ( edgesM, TpConfFmt, TpEdgeNo, TpAnglePack, debugLogAngles )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Function ( fix )


findAngle :: (TpConfFmt, TpEdgeNo) -> TpAnglePack
findAngle = findAngleSub2 1 . findAngleSub1 1 . findAngleSub0


-- ======== findangleSub0 ========
findAngleSub0 :: (TpConfFmt, TpEdgeNo) -> TpAnglePack
findAngleSub0 (gConf, edgeNo) = (gConf, edgeNo, angle3, diffAngle3, sameAngle0, contract2) where
  contract0  = replicate (edgesM + 1) 0
  angle0     = replicate edgesM $ replicate 5 0
  diffAngle0 = replicate edgesM $ replicate 5 0
  sameAngle0 = replicate edgesM $ replicate 5 0

  contract1 = contract0  & ix 0     .~ gConf !! 2 !! 0 -- number of edges in contract
  contract2 = contract1  & ix edgesM .~ gConf !! 1 !! 3

  edge       = 3 * gConf !! 1 !! 0 - 3 - gConf !! 1 !! 1
  diffAngle1 = diffAngle0  & (ix 0 <<< ix 0) .~ gConf !! 1 !! 0
  diffAngle2 = diffAngle1  & (ix 0 <<< ix 1) .~ gConf !! 1 !! 1
  diffAngle3 = diffAngle2  & (ix 0 <<< ix 2) .~ edge
  angle1     = angle0      & (ix 0 <<< ix 0) .~ diffAngle3 !! 0 !! 0
  angle2     = angle1      & (ix 0 <<< ix 1) .~ diffAngle3 !! 0 !! 1
  angle3     = angle2      & (ix 0 <<< ix 2) .~ diffAngle3 !! 0 !! 2


-- ======== findangleSub1 ========
findAngleSub1 :: Int -> TpAnglePack -> TpAnglePack
findAngleSub1 i pack@(gConf, edgeNo, angle, diffAngle, sameAngle, contract)
  | i > contract !! 0 = debugLogAngles (show sameAngle) pack
  | otherwise         = findAngleSub1 (i + 1) (gConf, edgeNo, angle, diffAngle, sameAngle, contract2) where
      u = gConf !! 2 !! (2 * i - 1)
      v = gConf !! 2 !! (2 * i)
      contract2 = if edgeNo !! u !! v < 1 then error ("***  ERROR: CONTRACT CONTAINS NON-EDGE  *** " ++ show u ++ " " ++ show v) else contract & ix (edgeNo !! u !! v) .~ 1



-- ======== findangleSub2 ========
findAngleSub2 :: Int -> TpAnglePack -> TpAnglePack
findAngleSub2 v pack@(gConf, _, _, _, sameAngle, _)
  | v > gConf !! 1 !! 0 = pack --debugLogAngles (show sameAngle) pack
  | otherwise           = findAngleSub2 (v + 1) nextPack where
      nextPack = flip fix (pack, 1) $ \loop (pack2@(gc, ed, _, _, _, _co), h) -> case () of
                  _ | h > gc !! (v + 2) !! 1                       ->       pack2         -- end
                    | v <= gc !! 1 !! 1 && h == gc !! (v + 2) !! 1 -> loop (pack2, h + 1) -- next
                    | h >= length (gc !! (v + 2))                  ->       pack2         -- break
                    | otherwise                                    -> loop (pack3, h + 1) where
                        i = if h < gc !! (v + 2) !! 1 then h + 1 else 1
                        u = gc !! (v + 2) !! (h + 1)
                        w = gc !! (v + 2) !! (i + 1)
                        a = ed !! v !! w
                        b = ed !! u !! w
                        -- どっちかが0なら通過
                        c = if 0 /= _co !! a && 0 /= _co !! b then error "***  ERROR: CONTRACT IS NOT SPARSE  ***" else ed !! u !! v
                        --c = ed !! u !! v
                        pack3 = debugLogAngles (show sameAngle) $ (setAngle b a c . setAngle a b c) pack2


setAngle :: Int -> Int -> Int -> TpAnglePack -> TpAnglePack
setAngle x y c pack@(gc, ed, an, di, sa, co) =
  let
    an2   = an & (ix c <<< ix 0) .~ an !! c !! 0 + 1
    d     = an2 !! c !! 0
    an3   = an2 & (ix c <<< ix d) .~ x
    bool1 = 0 == co !! x && 0 == co !! y && 0 == co !! c
    di2   = di & (ix c <<< ix 0) .~ di !! c !! 0 + 1
    e     = di2 !! c !! 0
    di3   = di2 & (ix c <<< ix e) .~ x
    bool2 = 0 == co !! y
    sa2   = debugLogAngles (show (sa !! c !! 0 + 1)) $ sa & (ix c <<< ix 0) .~ sa !! c !! 0 + 1
    f     = sa2 !! c !! 0
    sa3   = debugLogAngles ("c f x: " ++ show c ++ " " ++ show f ++ " " ++ show x) $ sa2 & (ix c <<< ix f) .~ x
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
--findAngleSub3 :: TpAnglePack -> (TpAngle, TpAngle, TpAngle, [Int])
--findAngleSub3 (_, _, an, di, sa, co) = (an, di, sa, co)



