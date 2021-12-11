module ReLibFindlive where

import CoLibCConst   ( edges, TpAngle, power, simatchnumber )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Bits     ( Bits(shift, (.&.), (.|.)) )    
import Data.Function ( fix )


-- computes {\cal C}_0 and stores it in live. That is, computes codes of
-- colorings of the ring that are not restrictions of tri-colorings of the
-- free extension. Returns the number of such codes
findlive :: Int -> Int -> [Int] -> Int -> TpAngle -> [Int] -> Int -> IO (Int, [Int])
findlive ring bigno live ncodes angle _power extentclaim
  = findliveSub bigno live angle ring ed extentclaim ncodes j c3 forbidden2 0 0 where
      ed             = head angle !! 2
      c              = replicate edges 0
      j              = ed - 1
      c2             = c  & ix ed .~ 1
      c3             = c2 & ix j  .~ 2
      forbidden      = replicate edges 0
      forbidden2     = forbidden & ix j .~ 5


findliveSub :: Int -> [Int] -> TpAngle -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> Int -> Int -> IO (Int, [Int])
findliveSub _bigno live _angle _ring _ed _extentclaim _ncodes _j _c _forbidden _extent 262144
  -- = error "findlive_sub : It was not good though it was repeated 262144 times!"
  = return (-1, live)
findliveSub bigno live angle ring ed extentclaim ncodes j c forbidden extent cnt = do
  let (exit1, c2) = (True, c)
{-
      (exit1, c2) = flip fix c $ \loop c -> case () of
                      _ | (forbidden !! j) .&. (c !! j) == 0 -> (False, c)
                        | exitSub                            -> (True, c3)
                        | otherwise                          -> loop c2 where
                            c2            = c & ix j .~ shift (c !! j) 1
                            (exitSub, c3) = flip fix c $ \loop c -> case () of
                                              _ | 8 .&. (c !! j) == 0 -> (False, c)
                                                | j > ed - 1          -> (True, c) -- print_status ring, ncodes, extent, extentclaim
                                                | otherwise                          -> loop c2 where
                                                    c2 = c & ix j .~ shift (c !! j) 1
-}
  let (exit2, c3, j3) = (False, c, 0)
{- このブロックに問題あり
  (exit2, c3, j3) <- flip fix (c2, j) $ \loop (c, j) -> case () of
                      _ | 8 .&. (c !! j) == 0 -> return (False, c, j)
                        | j > ed -1           -> do{ printStatus ring ncodes extent extentclaim; return (True, c, j) }
                        | otherwise           -> loop (c2, j2) where
                            j2 = j + 1
                            c2 = c & ix j2 .~ shift (c !! j2) 1
-}
  let (exit3, _u, j4) = (False, 0, j)
{-
  let (exit3, _u, j4) = flip fix (0, j, head (angle !! j)) $ \loop (u, j, i) -> case () of
                _ | i > 4                 -> (True, c, u)
                  | i > head (angle !! j) -> (True, c, u)
                  | j < 0                 -> (False, c, u)
                  | otherwise             -> loop (u2, j - 1, i + 1) where
                      u2 = u .|. c !! ((angle !! j) !! i)
-}
  let (jNext, cNext) = if j == ring + 1 then (j3, c3) else (j4, c2)
  case () of
    _ | exit1                  -> return (ncodes - extent, live)
      | exit2 && j == ring + 1 -> return (ncodes - extent, live)
      | exit3 && j /= ring + 1 -> return (ncodes - extent, live)
      | otherwise ->
          findliveSub bigno live angle ring ed extentclaim ncodes jNext cNext forbidden extent (cnt + 1)


-- Given a colouring specified by a 1,2,4-valued function "col", it computes
-- the corresponding number, checks if it is in live, and if so removes it.
record :: [Int] -> Int -> TpAngle -> Int -> Int -> [Int] -> (Int, [Int])
record col ring angle bigno extent live
  | live !! colno /= 0 = (extent + 1, live & ix colno .~ 0)
  | otherwise          = (extent    , live) where
      colno      = bigno - 2 * min - max
      weight0    = [0, 0, 0, 0, 0]
      weight     = flip fix (weight0, 1) $ \loop (weight, i) -> case () of
                    _ | i > ring  -> weight
                      | otherwise -> loop (weight1, i + 1) where
                          sum     = 7 - col !! ((angle !! i) !! 1) - col !! ((angle !! i) !! 2)
                          sum2    = if sum >= 5 then 4 else sum
                          sum3    = if sum2 <= -1 then 0 else sum2
                          weight1 = weight & ix sum3 .~ (weight !! sum3 + power !! i)
      (min, max) = flip fix (weight !! 4, weight !! 4, 0) $ \loop (i, j, t) -> case () of
                    _ | t >= 2    -> (i, j)
                      | w < i     -> loop (w, j, t + 1)
                      | otherwise -> loop (i, w, t + 1) where
                          w = weight !! (t + 1)


printStatus :: Int -> Int -> Int -> Int -> IO ()
printStatus ring totalcols extent extentclaim = do
  putStr
    $ "\n\n   This has ring-size "
    ++ show ring
    ++ ", so there are "
    ++ show totalcols
    ++ " colourings total,\n"
  putStr   $ "   and " ++ show (simatchnumber !! ring) ++ " balanced signed matchings.\n"
  -- putStr   $ "\n   There are " ++ show extent ++ " colourings that extend to the configuration."   !!!bug3!!!
  putStr     "\n\n            remaining               remaining balanced\n"
  putStr     "           colourings               signed matchings\n"
  -- putStrLn $ "\n              " ++ show (totalcols - extent)



