module ReLibFindlive( findLive ) where

import CoLibCConst   ( edgesM, TpAngle, power, siMatchNumber, TpExtCJ, TpFliveBindPack, TpBPSPack )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Bits     ( Bits(shift, (.&.), (.|.)) )
import Data.Function ( fix )


-- computes {\cal C}_0 and stores it in live. That is, computes codes of
-- colorings of the ring that are not restrictions of tri-colorings of the
-- free extension. Returns the number of such codes
findLive :: Int -> Int -> [Int] -> Int -> TpAngle -> Int -> (Int, [Int])
findLive ring bigNo live nCodes angle extentC
  = findLiveSub (ring, nCodes, 0, extentC, (False, c3, j), ed) bigNo live angle forbi2 0 where
      ed         = head angle !! 2
      c          = replicate edgesM 0
      j          = ed - 1
      c2         = c  & ix ed .~ 1
      c3         = c2 & ix j  .~ 2
      forbi      = replicate edgesM 0
      forbi2     = forbi & ix j .~ 5


-- ======== findliveSub ========
findLiveSub :: TpBPSPack -> Int -> [Int] -> TpAngle -> [Int] -> Int -> (Int, [Int])
findLiveSub _ _ _ _ _ 262144 = error "findlive_sub : It was not good though it was repeated 262144 times!意図的なエラー"
findLiveSub (ring, nCodes, extent, extentC, (_, c, j), ed) bigNo live angle forbi cnt =
  let ((exit1, _, j2), (extent2, live2), (exit2, _, _), (exit3, _, _), (_, cNext, jNext), forbi2)
        = (findLiveSsub5 ring
            . findLiveSsub4 angle ring
              . findLiveSsub3 (ring, nCodes, extent, extentC, (False, c, j), ed)
                . findLiveSsub2 angle bigNo ring
                  . findLiveSsub1 (ring, nCodes, extent, extentC, (False, c, j), ed)) (forbi, live)
  -- totalling
  in case () of
    _ | exit1                   -> (nCodes - extent,  live)
      | exit2 && j2 == ring + 1 -> (nCodes - extent2, live2)
      | exit3 && j2 /= ring + 1 -> (nCodes - extent2, live2)
      | otherwise ->
          findLiveSub (ring, nCodes, extent2, extentC, (False, cNext, jNext), ed) bigNo live2 angle forbi2 (cnt + 1)


findLiveSsub1 :: TpBPSPack -> ([Int], [Int]) -> TpFliveBindPack
findLiveSsub1 (ring, nCodes, extent, extentC, (_, c0, j0), ed) (forbi, live) =
  flip fix (False, c0, j0) $ \loop (exitSub, c, j) -> case () of
    _ | exitSub                        -> ((True,  c, j), (extent, live), (False, [], 0), (False, [], 0), (False, [], 0), forbi)
      | (forbi !! j) .&. (c !! j) == 0 -> ((False, c, j), (extent, live), (False, [], 0), (False, [], 0), (False, [], 0), forbi)
      | otherwise                      -> loop $ beforePrintStatus (ring, nCodes, extent, extentC, (False, c, j), ed)


findLiveSsub2 :: TpAngle -> Int -> Int -> TpFliveBindPack -> TpFliveBindPack
findLiveSsub2 angle bigNo ring
  ((exit1, c2, j2), (extent, live), second, third, fourth, forbi) =
    let (extent2, live2) = if j2 == ring + 1 then record c2 ring angle bigNo extent live else (extent, live)
    in ((exit1, c2, j2), (extent2, live2), second, third, fourth, forbi)


findLiveSsub3 :: TpBPSPack -> TpFliveBindPack -> TpFliveBindPack
findLiveSsub3 (ring, nCodes, _, extentC, _, ed) ((exit1, c2, j2), (extent, live), _, third, fourth, forbi) =
  let
    second = if j2 == ring + 1 then beforePrintStatus (ring, nCodes, extent, extentC, (False, c2, j2), ed)
              else (False, c2, j2)
  in
    ((exit1, c2, j2), (extent, live), second, third, fourth, forbi)


findLiveSsub4 :: TpAngle -> Int -> TpFliveBindPack -> TpFliveBindPack
findLiveSsub4 angle ring ((exit1, c2, j2), exLive, second, _, fourth, forbi) =
  let c4     = c2 & ix (j2 - 1) .~ 1
      j4     = j2 - 1
      exit3  = j4 < 0
      u3     = foldl (\x y -> x .|. c4 !! y) 0 $ tail (angle !! j4)
      forbi2 = if j2 == ring + 1 then forbi else forbi & ix j4 .~ u3
  in ((exit1, c2, j2), exLive, second, (exit3, c4, j4), fourth, forbi2)


findLiveSsub5 :: Int -> TpFliveBindPack -> TpFliveBindPack
findLiveSsub5 ring ((exit1, c2, j2), (extent, live), (exit2, c3, j3), (exit3, c4, j4), _, forbi) =
  let fourth = if j2 == ring + 1 then (False, c3, j3) else (False, c4, j4)
  in ((exit1, c2, j2), (extent, live), (exit2, c3, j3), (exit3, c4, j4), fourth, forbi)




-- ======== record ========
-- Given a colouring specified by a 1,2,4-valued function "col", it computes
-- the corresponding number, checks if it is in live, and if so removes it.
record :: [Int] -> Int -> TpAngle -> Int -> Int -> [Int] -> (Int, [Int])
record col ring angle bigNo extent live
  | live !! colNo /= 0 = (extent + 1, live & ix colNo .~ 0)
  | otherwise          = (extent    , live) where
      weight0      = [0, 0, 0, 0, 0]
      weight1      = flip fix (weight0, 1) $ \loop (weight, i) -> case () of
                      _ | i > ring  -> weight
                        | otherwise -> loop (weight2, i + 1) where
                            sum1    = 7 - col !! (angle !! i !! 1) - col !! (angle !! i !! 2)
                            sum2    = if sum1 >= 5  then 4 else sum1
                            sum3    = if sum2 <= -1 then 0 else sum2
                            weight2 = weight & ix sum3 .~ (weight !! sum3 + power !! i)
      (min0, max0) = flip fix (weight1 !! 4, weight1 !! 4, 1) $ \loop (i, j, t) -> case () of
                      _ | t > 2     -> (i, j)
                        | w < i     -> loop (w, j, t + 1)
                        | w > j     -> loop (i, w, t + 1)
                        | otherwise -> loop (i, j, t + 1) where
                            w = weight1 !! t
      colNo        = bigNo - 2 * min0 - max0




-- ======== printStatus ========
beforePrintStatus :: TpBPSPack -> TpExtCJ
beforePrintStatus (_ring, _nCodes, _extent, _extentC, (_, c, j), ed) =
  let c2 = c & ix j .~ shift (c !! j) 1
  in flip fix (c2, j) $ \loop (c0, j0) -> case () of
      _ | 8 .&. (c0 !! j0) == 0 -> (False, c0, j0)
        | j0 >= ed - 1        -> (True, c0, j0)
        | otherwise           -> loop (c3, j3) where
            j3 = j0 + 1
            c3 = c0 & ix j3 .~ shift (c0 !! j3) 1


_printStatus :: Int -> Int -> Int -> Int -> IO ()
_printStatus ring totalCols extent _ = do
  putStr
    $ "\n\n   This has ring-size "
    ++ show ring
    ++ ", so there are "
    ++ show totalCols
    ++ " colourings total,\n"
  putStr   $ "   and " ++ show (siMatchNumber !! ring) ++ " balanced signed matchings.\n"
  putStr   $ "\n   There are " ++ show extent ++ " colourings that extend to the configuration."
  putStr     "\n\n            remaining               remaining balanced\n"
  putStr     "           colourings               signed matchings\n"
  putStr   $ "\n              " ++ show (totalCols - extent)



