{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module ReLibCRedu( checkCReduce ) where

import CoLibCConst
    ( edges,
      power)
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Data.Bits                      ( Bits(shift, (.&.), (.|.)) )
import Data.Function                  ( fix )


checkCReduce :: Int -> Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> [Int] -> IO Bool
checkCReduce ring bigno nLive live diffangle sameangle contract = do
  _ <- if contract !! 0 /= 0         then return True else error "       ***  ERROR: NO CONTRACT PROPOSED  ***\n\n"
  _ <- if nLive == contract !! edges then return True else error " ***  ERROR: DISCREPANCY IN EXTERIOR SIZE  ***\n\n"

  let
    start      = diffangle !! 0 !! 2
    c          = replicate edges 0
    forbidden  = replicate edges 0 -- called F in the notes
    start2     = start - (length . takeWhile (/=0)) contract
    c2         = c & ix start2 .~ 1
    j          = start2 - 1
    j2         = j     - (length . takeWhile (/=0)) contract
    dm         = diffangle !! j2
    sm         = sameangle !! j2
    c3         = c2 & ix j2 .~ 1
    u          = 4
    imax1      = dm !! 0 - 1
    u2         = foldl (\x y -> x .|. c3 !! y) u  (take imax1 . tail $ dm)
    imax2      = sm !! 0 - 1
    u3         = foldl (\x y -> x .|. c3 !! y) u2 (take imax2 . tail $ sm)
    forbidden2 = forbidden & ix j2 .~ u3

  checkCReduceSub 0 forbidden2 c3 contract j2 start2 diffangle sameangle bigno ring live


checkCReduceSub :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> IO Bool
checkCReduceSub 2097152 _ _ _ _ _ _ _ _ _ _      = error "checkCReduceSub : It was not good though it was repeated 2097152 times!"
checkCReduceSub cnt forbidden c contract j start diffangle sameangle bigno ring live
  | j1 == one && not (inLive c1 ring live bigno) = error "ERROR: INPUT CONTRACT IS INCORRECT  ***\n\n"
  | j1 == 1 && ret2                              = do {putStrLn "               ***  Contract confirmed ***"; return True}
  | j1 <= 0                                      = error "checkCReduceSub : error!"
  | otherwise                                    = checkCReduceSub (cnt + 1) forbidden c2 contract j2 start diffangle sameangle bigno ring live where
      one = 1::Int
      (_,    c1, j1) = (True, c, 1::Int) --ccrSubSub1 c  j  contract start forbidden
      (ret2, c2, j2) = (True, c, j) --ccrSubSub2 c1 j1 contract start


-- bug有り!!
ccrSubSub1 :: [Int] -> Int -> [Int] -> Int -> [Int] -> (Bool, [Int], Int)
ccrSubSub1 c j contract start forbidden
  | forbidden !! j .&. c !! j == 0 = (False, c, j)
  | ret2                           = (True, c2, j2)
  | otherwise                      = ccrSubSub1 c2 j2 contract start forbidden where
      (ret2, c2, j2) = ccrSubSub2 c j contract start


ccrSubSub2 :: [Int] -> Int -> [Int] -> Int -> (Bool, [Int], Int)
ccrSubSub2 c j contract start
  | c2 !! j .&. 8 == 0 = (False, c2, j)
  | j2 >= start        = (True, c2, j2)
  | otherwise          = ccrSubSub2 (c2 & ix j2 .~ shift (c2 !! j2) (-1)) j2 contract start where
      c2 = c & ix j .~ shift (c !! j) (-1)
      j2 = j - (length . takeWhile (/=0)) contract


inLive :: [Int] -> Int -> [Int] -> Int -> Bool
inLive col ring live bigno = True
{-
  | length live <= colno = error "inLive length over!!"
  | live !! colno == 0   = True
  | otherwise            = False where
      colno        = bigno - 2 * min0 - max0
      (min0, max0) = flip fix (weight !! 4, weight !! 4, 1) $ \loop (min1, max1, i) -> case () of
                      _ | i > 2     -> (min1, max1)
                        | otherwise -> loop (min2, max2, i + 1) where
                            w    = weight !! i
                            min2 = min w min0
                            max2 = max w max0
      weight0      = replicate 5 0
      weight       = flip fix (weight0, 1) $ \loop (weight1, i) -> case () of
                      _ | i > ring  -> weight1
                        | otherwise -> loop (weight2, i + 1) where
                            i2      = if i >= edges then edges - 1 else i
                            colI    = if col !! i2 > 4 then 4 else col !! i2
                            weight2 = weight1 & ix colI .~ weight1 !! colI + power !! i
-}



