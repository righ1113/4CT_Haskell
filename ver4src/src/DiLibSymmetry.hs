module DiLibSymmetry where

import DiLibCConst               ( TpPosoutI, TpPosout, TpAxleI )


delSym :: Int -> [Int] -> Int -> Int
delSym nosym nolines lev =
  let loop1 i
        | i < 1 || nolines !! (nosym - 1) - 1 < lev = i
        | otherwise                                 = loop1 (i - 1)
  in loop1 nosym

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

{-
checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, upp, lev) posout@(number, nolines, value, pos, plow, pupp, xx) nosym =
  let [k, epsilon, level, line] = map read str :: [Int]
      i                         = fromJust $ find (==line) number
      pI                        = (number !! i, nolines !! i, value !! i, pos !! i, plow !! i, pupp !! i, xx !! i)
    -- ★mark
  in if k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1 then                       error "Illegal symmetry"
      else if i >= nosym then                                                                    error "No symmetry as requested"
          else if nolines !! i /= level + 1 then                                                error "Level mismatch"
                else if epsilon == 0 && outletForced (low !! lev, upp !! lev) pI (k+1) /= 1 then error "Invalid symmetry"
                    else if reflForced aA /= 1 then                                             error "Invalid reflected symmetry"
                                                else                                             putStrLn "  checkSymmetry OK."

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int
outletForced aAI@(lowI, uppI) (numberI, nolinesI, valueI, posI, plowI, puppI, xxI) y =
  let deg = head lowI
      y2  = y - 1
      f n x cont
        | n == 0    = n
        | otherwise = cont $ outletForcedSub aAI deg posI y2 n x
      ret           = myLoop f id 1 [0, 1 .. nolinesI]
  in if ret == 0
    then 0
    else valueI
outletForcedSub :: TpAxleI -> Int -> [Int] -> Int -> Int -> Int -> Int
outletForcedSub aAI@(lowI, uppI) deg posI y _ i =
  let p  = posI !! i
      p2 = if (y + (p - 1) `mod` deg) < deg then p + y else p + y - deg
  in if (lowI !! i > lowI !! p2) || (uppI !! i < uppI !! p2)
    then 0
    else 1

reflForced :: TpAxle -> Int
reflForced _ = 1
-}



