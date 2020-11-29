module DiLibSymmetry where

import DiLibCConst ( TpAxle, TpAxleI, TpPosout, TpPosoutI )
import Data.List   ( findIndex )
import Data.Maybe  ( fromJust )


checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> Int -> IO ()
checkSymmetry str aA@(low, upp, lev) posout@(number, nolines, _, _, _, _) nosym deg = do
  let
    [k, epsilon, level, line] = map read str :: [Int]
    i                         = ((fromJust .) . findIndex) (== line) number
    ret
      | k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1                               = error "Illegal symmetry"
      | i >= nosym                                                                                 = error "No symmetry as requested"
      | nolines !! i /= level + 1                                                                  = error "Level mismatch"
      | epsilon == 0 && outletForced (low !! lev, upp !! lev) (getPosoutI posout i) (k+1) deg /= 1 = error "Invalid symmetry"
      | reflForced aA /= 1                                                                         = error "Invalid reflected symmetry"
      | otherwise                                                                                  = putStrLn "  checkSymmetry OK."
  putStrLn $ show i ++ ", " ++ show nosym
  ret


delSym :: Int -> [Int] -> Int -> Int
delSym nosym nolines lev
  | nosym < 1 || nolines !! (nosym - 1) - 1 < lev = nosym
  | otherwise                                     = delSym (nosym - 1) nolines lev

getPosoutI :: TpPosout -> Int -> TpPosoutI
getPosoutI (num, nol, val, pos, low, upp) i
  = (num !! i, nol !! i, val !! i, pos !! i, low !! i, upp !! i)

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
outletForced (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let p1 = posI !! i
              p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
          in if lowI !! i > axLowL !! p2 || uppI !! i < axUppL !! p2 then
            0
          else
            loop1 (i + 1)
  in loop1 0

outletPermitted :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
outletPermitted (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let p1 = posI !! i
              p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
          in if lowI !! i > axUppL !! p2 || uppI !! i < axLowL !! p2 then
            0
          else
            loop1 (i + 1)
  in loop1 0


reflForced :: TpAxle -> Int
reflForced _ = 1




