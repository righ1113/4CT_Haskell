module DiLibCaseSplit( caseSplit1, caseSplit2 ) where

import CoLibCConst               ( TpPosout, TpCond, TpAxle, infty )
import Control.Arrow             ( (<<<) )
import Control.Lens              ( (&), (^?!), (.~), Ixed(ix) )
import Data.List                 ( find )
import Data.Maybe                ( isNothing )


caseSplit1 :: TpCond -> TpAxle -> Int -> Int -> TpAxle
caseSplit1 _ (low, upp, lev) n m =
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
              ,lev + 1 )
        | m <= 0 &&    (aLowN > -m || -m >= aUppN) = error "Invalid upper bound in condition"
        | otherwise                                =
            -- new upper bound
            ( low2 & (ix lev        <<< ix n) .~ (1 - m)
              ,upp2 & (ix (lev + 1) <<< ix n) .~ (-m)
              ,lev + 1 )
  in ret

caseSplit2 :: TpCond -> TpAxle -> Int -> TpPosout -> Int -> Int -> (TpPosout, Int)
caseSplit2 (nn, mm) (_, _, lev) deg sym@(symNum, symNol, symVal, symPos, symLow, symUpp) nosym lineno =
  -- good : 更新する
  let bad  = find (`notElem` [1..(2 * deg)]) (take (lev+1) nn)
      num  = symNum & ix nosym .~ lineno
      val  = symVal & ix nosym .~ 1
      nol  = symNol & ix nosym .~ (lev + 1)
      loop1 :: Int -> ([[Int]], [[Int]], [[Int]]) -> ([[Int]], [[Int]], [[Int]])
      loop1 i (pos0, low0, upp0)
        | i > lev   = (pos0, low0, upp0)
        | otherwise =
            let pos2 = pos0 & (ix nosym <<< ix i) .~ (nn !! i)
                low2
                  | mm !! i > 0 = low0 & (ix nosym <<< ix i) .~ (mm !! i)
                  | otherwise   = low0 & (ix nosym <<< ix i) .~ 5
                upp2
                  | mm !! i > 0 = upp0 & (ix nosym <<< ix i) .~ infty
                  | otherwise   = upp0 & (ix nosym <<< ix i) .~ -(mm !! i)
            in loop1 (i + 1) (pos2, low2, upp2)
      (pos, low, upp) = loop1 0 (symPos, symLow, symUpp)
  in if isNothing bad then
    ((num, nol, val, pos, low, upp), nosym + 1)
  else
    (sym, nosym)



