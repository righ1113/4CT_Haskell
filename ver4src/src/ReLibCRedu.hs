{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module ReLibCRedu where

import CoLibCConst
    ( edges,
      siMatchNumber,
      power,
      maxRing,
      --debugLogUpdateLive,
      TpRealityPack,
      TpBaseCol,
      TpTMbind,
      TpUpdateState2,
      TpLiveTwin,
      TpRingNchar )
import Control.Applicative            ( empty )
import Control.Arrow                  ( (<<<) )
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Control.Monad.Trans.Class      ( lift )
import Control.Monad.Trans.Maybe      ( MaybeT(..) )
import Control.Monad.Trans.State.Lazy ( StateT(..), execStateT, get, put )
import Data.Bits                      ( Bits(shift, (.&.), (.|.), xor) )    
import Data.Function                  ( fix )
import Data.Int                       ( Int8 )
import Data.Maybe                     ( isNothing, fromJust )


checkCReduce :: Int -> Int -> Int -> [Int] -> [[Int]] -> [[Int]] -> [Int] -> IO Bool
checkCReduce ring bigno nLive live diffangle sameangle contract = do
  if contract !! 0 /= 0         then return True else error "       ***  ERROR: NO CONTRACT PROPOSED  ***\n\n"
  if nLive == contract !! edges then return True else error " ***  ERROR: DISCREPANCY IN EXTERIOR SIZE  ***\n\n"

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
checkCReduceSub 2097152 _ _ _ _ _ _ _ _ _ _ = error "checkCReduceSub : It was not good though it was repeated 2097152 times!"
checkCReduceSub cnt forbidden c contract j start diffangle sameangle bigno ring live
  | j1 == 1 && ret2 = do {putStrLn "               ***  Contract confirmed ***"; return True}
  | otherwise       = checkCReduceSub (cnt + 1) forbidden c2 contract j2 start diffangle sameangle bigno ring live where
      (ret1, c1, j1) = ccrSubSub c  j  contract start
      (ret2, c2, j2) = ccrSubSub c1 j1 contract start


ccrSubSub :: [Int] -> Int -> [Int] -> Int -> (Bool, [Int], Int)
ccrSubSub c j contract start
  | c2 !! j .&. 8 == 0 = (False, c2, j)
  | j2 >= start        = (True, c2, j2)
  | otherwise          = ccrSubSub (c2 & ix j2 .~ shift (c2 !! j2) (-1)) j2 contract start where
      c2 = c & ix j .~ shift (c !! j) (-1)
      j2 = j - (length . takeWhile (/=0)) contract



