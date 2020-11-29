module DiLibCondition where

import DiLibCConst               ( TpPosout, TpCond, TpAxle, infty )
import Control.Arrow             ( (<<<) )
import Control.Lens              ( (&), (^?!), (.~), Ixed(ix) )
import Data.List                 ( find )
import Data.Maybe                ( isNothing )


checkCondition1 :: TpCond -> TpAxle -> Int -> Int -> TpAxle
checkCondition1 _ (low, upp, lev) n m =
  let low2  = low & ix (lev + 1) .~ (low ^?! ix lev)
      upp2  = upp & ix (lev + 1) .~ (upp ^?! ix lev)
      aLowN = (low2 ^?! ix lev) ^?! ix n
      aUppN = (upp2 ^?! ix lev) ^?! ix n
      ret
        | m > 0 &&     (aLowN >= m || m > aUppN)   = error "Invalid lower bound in condition"
        | m > 0 && not (aLowN >= m || m > aUppN)   =
            -- new lower bound
            ( low2 & (ix (lev + 1) <<< ix n) .~ m
              ,upp2 & (ix lev      <<< ix n) .~ (m - 1)
              ,lev )
        | m <= 0 &&    (aLowN > -m || -m >= aUppN) = error "Invalid upper bound in condition"
        | otherwise                                =
            -- new upper bound
            ( low2 & (ix lev        <<< ix n) .~ (1 - m)
              ,upp2 & (ix (lev + 1) <<< ix n) .~ (-m)
              ,lev )
  in ret

checkCondition2 :: TpCond -> TpAxle -> Int -> TpPosout -> Int -> Int -> (TpPosout, Int)
checkCondition2 (nn, mm) (_, _, lev) deg sym@(symNum, symNol, symVal, symPos, symLow, symUpp) nosym lineno =
  let good = find (\ x -> x > 2 * deg || x < 1) (take (lev+1) nn)
      num  = symNum & ix nosym .~ lineno
      val  = symVal & ix nosym .~ 1
      nol  = symNol & ix nosym .~ (lev + 1)
      loop1 :: Int -> ([[Int]], [[Int]], [[Int]]) -> ([[Int]], [[Int]], [[Int]])
      loop1 i (pos, low, upp)
        | i > lev   = (pos, low, upp)
        | otherwise =
            let pos2 = pos & (ix nosym <<< ix i) .~ (nn !! i)
                low2
                  | mm !! i > 0 = low & (ix nosym <<< ix i) .~ (mm !! i)
                  | otherwise   = low & (ix nosym <<< ix i) .~ 5
                upp2
                  | mm !! i > 0 = upp & (ix nosym <<< ix i) .~ infty
                  | otherwise   = upp & (ix nosym <<< ix i) .~ -(mm !! i)
            in loop1 (i + 1) (pos2, low2, upp2)
      (pos, low, upp) = loop1 0 (symPos, symLow, symUpp)
  in if isNothing good then
    ((num, nol, val, pos, low, upp), nosym + 1)
  else
    (sym, nosym)



