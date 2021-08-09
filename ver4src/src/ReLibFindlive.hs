module ReLibFindlive where

import CoLibCConst   ( edges, TpAngle )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Bits     ( Bits(shift, (.&.), (.|.)) )    


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
findliveSub bigno live angle ring ed extentclaim ncodes j _c forbidden extent cnt
  | exit      = return (ncodes - extent, live)
  | otherwise = findliveSub bigno live angle ring ed extentclaim ncodes j c2 forbidden extent (cnt + 1) where
      (exit, c2) = (False, c2) --findliveSubSub1 forbidden j c ed ring ncodes extent extentclaim angle


findliveSubSub1 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> TpAngle -> (Bool, [Int])
findliveSubSub1 forbidden j c ed ring ncodes extent extentclaim angle
  | (forbidden !! j) .&. (c !! j) == 0 = (False, c)
  | exit                               = (True, c3)
  | otherwise                          = findliveSubSub1 forbidden j c3 ed ring ncodes extent extentclaim angle where
      c2         = c & ix j .~ shift (c !! j) 1
      (exit, c3) = findliveSubSub1Sub forbidden j c2 ed ring ncodes extent extentclaim


findliveSubSub1Sub :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
findliveSubSub1Sub forbidden j c ed ring ncodes extent extentclaim
  | 8 .&. (c !! j) == 0 = (False, c)
  | j > ed -1           = (True, c) -- print_status ring, ncodes, extent, extentclaim
  | otherwise           = findliveSubSub1Sub forbidden j c2 ed ring ncodes extent extentclaim where
      c2 = c & ix j .~ shift (c !! j) 1


findliveSubSub2 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
findliveSubSub2 forbidden j c ed ring ncodes extent extentclaim
  | 8 .&. (c !! j) == 0 = (False, c)
  | j > ed -1           = (True, c) -- print_status ring, ncodes, extent, extentclaim
  | otherwise           = findliveSubSub2 forbidden j2 c2 ed ring ncodes extent extentclaim where
      j2 = j + 1
      c2 = c & ix j2 .~ shift (c !! j2) 1

--          jjj -= 1
findliveSubSub3 :: [Int] -> Int -> [Int] -> Int -> Int -> Int -> Int -> Int -> TpAngle -> Int -> Int -> (Bool, [Int], Int)
findliveSubSub3 forbidden j c ed ring ncodes extent extentclaim angle i u
  | i > 4                 = (True, c, u)
  | i > head (angle !! j) = (True, c, u)
  | j < 0                 = (False, c, u)
  | otherwise             = findliveSubSub3 forbidden j c ed ring ncodes extent extentclaim angle (i + 1) u2 where
      u2 = u .|. c !! ((angle !! j) !! i)
{-
          return [ncodes - extent, @live] if jjj.negative?
          ccc[jjj] = 1
          u = 0
          angle[jjj][0].times do |ii|
            i = ii + 1
            break 0 if i >= 5
            u |= ccc[angle[jjj][i]]
          end
          forbidden[jjj] = u
-}

