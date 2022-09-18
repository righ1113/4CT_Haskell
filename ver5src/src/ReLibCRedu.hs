{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module ReLibCRedu( checkCReduce ) where

import CoLibCConst
    ( edges,
      power )
import Control.Applicative            ( empty )
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Control.Monad.IO.Class         ( liftIO )
import Control.Monad.Trans.Maybe      ( MaybeT(..) )
import Data.Bits                      ( Bits(shift, (.&.), (.|.)), complement )
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
    imax1      = dm !! 0
    u2         = foldl (\x y -> x .|. c3 !! y) u  (take imax1 . tail $ dm)
    imax2      = sm !! 0
    u3         = foldl (\x y -> x .|. c3 !! y) u2 (take imax2 . tail $ sm)
    forbidden2 = forbidden & ix j2 .~ u3

  putStrLn ("f c j: " ++ show forbidden ++ " " ++ show c3 ++ " " ++ show j2)
  _ <- runMaybeT $ checkCReduceSub 0 forbidden2 c3 contract j2 start2 diffangle sameangle bigno ring live
  return True


checkCReduceSub :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> MaybeT IO Bool
checkCReduceSub 2097152 _ _ _ _ _ _ _ _ _ _      = error "checkCReduceSub : It was not good though it was repeated 2097152 times!"
checkCReduceSub cnt forbidden c contract j start diffangle sameangle bigno ring live = do
  --liftIO $ putStrLn "checkCReduceSub start."
  (c1, j1) <- ccrSubSub1 0 c  j  contract start forbidden
  --let (c1, j1) = (c, j)
  --liftIO $ putStrLn "checkCReduceSub chk 1."
  _        <- if j1 == 1 && not (inLive c1 ring live bigno) then error "ERROR: INPUT CONTRACT IS INCORRECT  ***\n\n" else return True
  (c2, j2) <- ccrSubSub2 0 (c1 & ix j1 .~ shift (c1 !! j1) 1) j1 contract start
  _        <- if j1 == 1 then checkCReduceSub (cnt + 1) forbidden c2 contract j2 start diffangle sameangle bigno ring live else return True
  _        <- if j2 <= 0 then error "checkCReduceSub : error!" else return True
  let
    j3         = ccrSubSub3 j1 contract
    dm         = diffangle !! j3
    sm         = sameangle !! j3
    c3         = c1 & ix j3 .~ 1
    u1         = 0
    u2         = ccrSubSub4 u1 c3 dm sm 1
    forbidden2 = forbidden & ix j3 .~ u2
  checkCReduceSub (cnt + 1) forbidden2 c3 contract j3 start diffangle sameangle bigno ring live


ccrSubSub1 :: Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> MaybeT IO ([Int], Int)
ccrSubSub1 100000 _ _ _ _ _ = error "ccrSubSub1 rec error!!"
ccrSubSub1 cnt c j contract start forbidden
  | b         = return (c, j)
  | otherwise = do
      --liftIO $ putStrLn ("f c j: " ++ show forbidden ++ " " ++ show c ++ " " ++ show j)
      (c3, j2) <- ccrSubSub2 0 c2 j contract start
      ccrSubSub1 (cnt + 1) c3 j2 contract start forbidden where
        b  = True --forbidden !! j .&. c !! j == 0
        c2 = c & ix j .~ shift (c !! j) 1


ccrSubSub2 :: Int -> [Int] -> Int -> [Int] -> Int -> MaybeT IO ([Int], Int)
ccrSubSub2 100000 _ _ _ _ = error "ccrSubSub2 rec error!!"
ccrSubSub2 cnt c j contract start
  | c2 !! j .&. 8 == 0 = return (c2, j)
  | j2 >= start        = do{ liftIO $ putStrLn "               ***  Contract confirmed ***"; empty }
  | otherwise          = do{ liftIO $ putStrLn "jkdhsfoiewrwelk"; ccrSubSub2 (cnt + 1) (c2 & ix j2 .~ shift (c2 !! j2) (-1)) j2 contract start } where
      c2 = c & ix j .~ shift (c !! j) 1
      j2 = j - (length . takeWhile (/=0)) contract


ccrSubSub3 :: Int -> [Int] -> Int
ccrSubSub3 j contract
  | j < 0              = 0
  | contract !! j == 0 = j
  | otherwise          = ccrSubSub3 (j - 1) contract


ccrSubSub4 :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int
ccrSubSub4 u c dm sm i
  | i > 4     = u
  | otherwise = ccrSubSub4 u3 c dm sm (i + 1) where
      u2 = if i <= dm !! 0 then u .|.             c !! (dm !! i)  else u
      u3 = if i <= sm !! 0 then u .|. complement (c !! (sm !! i)) else u2


inLive :: [Int] -> Int -> [Int] -> Int -> Bool
inLive col ring live bigno
{--}
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
{--}



