module DiLibApply( chkApply, delSym, getPosoutI, outletForced, outletPermitted, reflForced ) where

import CoLibCConst ( TpAxle, TpAxleI, TpPosout, TpPosoutI )
import Data.List   ( elemIndex )
import Data.Maybe  ( fromJust )


chkApply :: [String] -> TpAxle -> TpPosout -> Int -> Int -> IO ()
chkApply str (low, upp, lev) posout@(number, nolines, _, _, _, _) nosym deg = do
  let
    [k, epsilon, level, line] = map read str :: [Int]
    i                         = (fromJust . elemIndex line) number
    ret
      | k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1                               = error "Illegal symmetry"
      | i >= nosym                                                                                 = error "No symmetry as requested"
      | nolines !! i /= level + 1                                                                  = error "Level mismatch"
      | epsilon == 0 && outletForced (low !! lev, upp !! lev) (getPosoutI posout i) (k+1) deg /= 1 = error "Invalid symmetry"
      | epsilon /= 0 &&   reflForced (low !! lev, upp !! lev) (getPosoutI posout i) (k+1) deg /= 1 = error "Invalid reflected symmetry"
      | otherwise                                                                                  = putStrLn "  checkSymmetry OK."
  print posout
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

reflForced :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
reflForced (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let
            p1 = posI !! i
            p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
            q
              | p2 <=    deg =     deg - p2 + 1
              | p2 < 2 * deg = 3 * deg - p2
              | otherwise    = 2 * deg
          in if p2 < 1 || p2 > 2 * deg || lowI !! i > axLowL !! q || uppI !! i < axUppL !! q then
            0
          else
            loop1 (i + 1)
  in loop1 0



