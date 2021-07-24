module ReLibFindlive where

import CoLibCConst   ( edges, TpAngle )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Bits     ( Bits((.&.), shift) )


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
findliveSub bigno live angle ring ed extentclaim ncodes j c forbidden extent cnt
  | exit      = return (ncodes - extent, live)
  | otherwise = findliveSub bigno live angle ring ed extentclaim ncodes j c2 forbidden extent (cnt + 1) where
      (exit, c2) = (False, c2) --findliveSubSub1 forbidden j c ed ring ncodes extent extentclaim


findliveSubSub1 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
findliveSubSub1 forbidden j c ed ring ncodes extent extentclaim
  | (forbidden !! j) .&. (c !! j) == 0 = (False, c)
  | exit                               = (True, c3)
  | otherwise                          = findliveSubSub1 forbidden j c3 ed ring ncodes extent extentclaim where
      c2         = c & ix j .~ shift (c !! j) 1
      (exit, c3) = findliveSubSub1Sub forbidden j c2 ed ring ncodes extent extentclaim


findliveSubSub1Sub :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
findliveSubSub1Sub forbidden j c ed ring ncodes extent extentclaim
  | 8 .&. (c !! j) == 0 = (False, c)
  | j > ed -1           = (True, c) -- print_status ring, ncodes, extent, extentclaim
  | otherwise           = findliveSubSub1Sub forbidden j c2 ed ring ncodes extent extentclaim where
      c2 = c & ix j .~ shift (c !! j) 1



