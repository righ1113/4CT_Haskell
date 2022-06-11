module ReLibFindlive where

import CoLibCConst   ( edges, TpAngle, power, siMatchNumber, TpExtCJ, TpFliveBindPack, TpBPSPack )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Bits     ( Bits(shift, (.&.), (.|.)) )    
import Data.Function ( fix )


-- computes {\cal C}_0 and stores it in live. That is, computes codes of
-- colorings of the ring that are not restrictions of tri-colorings of the
-- free extension. Returns the number of such codes
findLive :: Int -> Int -> [Int] -> Int -> TpAngle -> [Int] -> Int -> IO (Int, [Int])
findLive ring bigNo live nCodes angle _ extentC
  = findLiveSub (ring, nCodes, 0, extentC, (False, c3, j), ed) bigNo live angle forbi2 0 where
      ed         = head angle !! 2
      c          = replicate edges 0
      j          = ed - 1
      c2         = c  & ix ed .~ 1
      c3         = c2 & ix j  .~ 2
      forbi      = replicate edges 0
      forbi2     = forbi & ix j .~ 5


-- ======== findliveSub ========
findLiveSub :: TpBPSPack -> Int -> [Int] -> TpAngle -> [Int] -> Int -> IO (Int, [Int])
findLiveSub _ _ _ _ _ 128 = error "findlive_sub : It was not good though it was repeated 262144 times!意図的なエラー"
findLiveSub (ring, nCodes, extent, extentC, (_, c, j), ed) bigNo live angle forbi cnt = do
  print angle
  putStrLn $ "cnt: " ++ show cnt
  -- ここは Applicative(<*>) では出来ないので、bind(=<<) でおこなう
  ((exit1, c2, j2), (extent2, live2), (exit2, c3, j3), (exit3, _, j4), (_, cNext, jNext), forbi2)
    <- findLiveSsub5 ring
      =<< findLiveSsub4 angle ring
        =<< findLiveSsub3 (ring, nCodes, extent, extentC, (False, c, j), ed)
          =<< findLiveSsub2 angle bigNo ring
            =<< findLiveSsub1 (ring, nCodes, extent, extentC, (False, c, j), ed) forbi live
  -- totalling
  case () of
    _ | exit1                   -> return (nCodes - extent,  live)
      | exit2 && j2 == ring + 1 -> return (nCodes - extent2, live2)
      | exit3 && j2 /= ring + 1 -> return (nCodes - extent2, live2)
      | otherwise ->
          findLiveSub (ring, nCodes, extent2, extentC, (False, cNext, jNext), ed) bigNo live2 angle forbi2 (cnt + 1)


findLiveSsub1 :: TpBPSPack -> [Int] ->  [Int] -> IO TpFliveBindPack
findLiveSsub1 (ring, nCodes, extent, extentC, (_, c, j), ed) forbi live =
  flip fix (False, c, j) $ \loop (exitSub, c, j) -> case () of
    _ | exitSub                        -> return ((True,  c, j), (extent, live), (False, [], 0), (False, [], 0), (False, [], 0), forbi)
      | (forbi !! j) .&. (c !! j) == 0 -> do{ print (forbi !! j); print (c !! j); print ((forbi !! j) .&. (c !! j));
                                              return ((False, c, j), (extent, live), (False, [], 0), (False, [], 0), (False, [], 0), forbi) }
      | otherwise                      -> beforePrintStatus (ring, nCodes, extent, extentC, (False, c, j), ed) >>= loop


findLiveSsub2 :: TpAngle -> Int -> Int -> TpFliveBindPack -> IO TpFliveBindPack
findLiveSsub2 angle bigNo ring
  ((exit1, c2, j2), (extent, live), second, third, fourth, forbi) = do
    let (extent2, live2) = if j2 == ring + 1 then record c2 ring angle bigNo extent live else (extent, live)
    return ((exit1, c2, j2), (extent2, live2), second, third, fourth, forbi)


findLiveSsub3 :: TpBPSPack -> TpFliveBindPack -> IO TpFliveBindPack
findLiveSsub3 (ring, nCodes, _, extentC, _, ed)
  ((exit1, c2, j2), (extent, live), _, third, fourth, forbi) = do
    second <- if j2 == ring + 1 then beforePrintStatus (ring, nCodes, extent, extentC, (False, c2, j2), ed)
              else return (False, c2, j2)
    return ((exit1, c2, j2), (extent, live), second, third, fourth, forbi)


findLiveSsub4 :: TpAngle -> Int -> TpFliveBindPack -> IO TpFliveBindPack
findLiveSsub4 angle ring ((exit1, c2, j2), exLive, second, _, fourth, forbi) = do
  let c4     = c2 & ix (j2 - 1) .~ 1
      j4     = j2 - 1
      exit3  = j4 < 0
      u3     = foldl (\x y -> x .|. c4 !! y) 0 $ tail (angle !! j4)
      forbi2 = if j2 == ring + 1 then forbi else forbi & ix j4 .~ u3
  -- putStrLn $ "forbi2: " ++ show (forbi2 !! j4) ++ "  " ++ show u3
  -- print c4
  -- print forbi2
  return ((exit1, c2, j2), exLive, second, (exit3, c4, j4), fourth, forbi2)


findLiveSsub5 :: Int -> TpFliveBindPack -> IO TpFliveBindPack
findLiveSsub5 ring ((exit1, c2, j2), (extent, live), (exit2, c3, j3), (exit3, c4, j4), _, forbi) = do
  let fourth = if j2 == ring + 1 then (False, c3, j3) else (False, c4, j4)
  return ((exit1, c2, j2), (extent, live), (exit2, c3, j3), (exit3, c4, j4), fourth, forbi)




-- ======== record ========
-- Given a colouring specified by a 1,2,4-valued function "col", it computes
-- the corresponding number, checks if it is in live, and if so removes it.
record :: [Int] -> Int -> TpAngle -> Int -> Int -> [Int] -> (Int, [Int])
record col ring angle bigNo extent live
  | live !! colNo /= 0 = (extent + 1, live & ix colNo .~ 0)
  | otherwise          = (extent    , live) where
      weight0    = [0, 0, 0, 0, 0]
      weight     = flip fix (weight0, 1) $ \loop (weight, i) -> case () of
                    _ | i > ring  -> weight
                      | otherwise -> loop (weight1, i + 1) where
                          sum     = 7 - col !! (angle !! i !! 1) - col !! (angle !! i !! 2)
                          sum2    = if sum  >= 5  then 4 else sum
                          sum3    = if sum2 <= -1 then 0 else sum2
                          weight1 = weight & ix sum3 .~ (weight !! sum3 + power !! i)
      (min, max) = flip fix (weight !! 4, weight !! 4, 1) $ \loop (i, j, t) -> case () of
                    _ | t > 2     -> (i, j)
                      | w < i     -> loop (w, j, t + 1)
                      | w > j     -> loop (i, w, t + 1)
                      | otherwise -> loop (i, j, t + 1) where
                          w = weight !! t
      colNo      = bigNo - 2 * min - max




-- ======== printStatus ========
beforePrintStatus :: TpBPSPack -> IO TpExtCJ
beforePrintStatus (ring, nCodes, extent, extentC, (_, c, j), ed) = do
  let c2 = c & ix j .~ shift (c !! j) 1
  putStrLn "here"
  print c2
  print j
  print ed
  print ring
  flip fix (c2, j) $ \loop (c, j) -> case () of
    _ | 8 .&. (c !! j) == 0 -> return (False, c, j)
      | j >= ed - 1         -> do{ printStatus ring nCodes extent extentC; return (True, c, j) }
      | otherwise           -> loop (c2, j2) where
          j2 = j + 1
          c2 = c & ix j2 .~ shift (c !! j2) 1


printStatus :: Int -> Int -> Int -> Int -> IO ()
printStatus ring totalCols extent _ = do
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



