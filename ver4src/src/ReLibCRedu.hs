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

  checkCReduceSub forbidden2 c3 contract j2 start2 diffangle sameangle bigno ring live


checkCReduceSub :: [Int] -> [Int] -> [Int] -> Int -> Int -> [[Int]] -> [[Int]] -> Int -> Int -> [Int] -> IO Bool
checkCReduceSub forbidden c contract j start diffangle sameangle bigno ring live = return True



