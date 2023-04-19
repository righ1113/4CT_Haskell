{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module DiLibApply( chkApply ) where

import CoLibCConst ( TpAxle, TpPosout, getPosoutI, outletForced, reflForced )
import Data.List   ( elemIndex )
import Data.Maybe  ( fromJust )
import Debug.Trace (trace)


chkApply :: [String] -> TpAxle -> TpPosout -> Int -> Int -> Int
chkApply str (low, upp, lev) posout@(number, nolines, _, _, _, _) nosym deg =
  let
    xs = map read str :: [Int]
    --[k, epsilon, level, line] = if length xs == 4 then xs else error "xs error."
    k       = xs !! 0
    epsilon = xs !! 1
    level   = xs !! 2
    line    = xs !! 3
    i                         = (fromJust . elemIndex line) number
    ret
      | k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1                               = error "Illegal symmetry"
      | i >= nosym                                                                                 = error "No symmetry as requested"
      | nolines !! i /= level + 1                                                                  = error "Level mismatch"
      | epsilon == 0 && outletForced (low !! lev, upp !! lev) (getPosoutI posout i) (k+1) deg /= 1 = error "Invalid symmetry"
      | epsilon /= 0 &&   reflForced (low !! lev, upp !! lev) (getPosoutI posout i) (k+1) deg /= 1 = error "Invalid reflected symmetry"
      | otherwise                                                                                  = trace "  checkSymmetry OK." deg
  --in trace ("    i= " ++ show i ++ " line= " ++ show line ++ show number) ret
  in ret



