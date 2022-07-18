{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module ReLibCRedu where

import CoLibCConst
    ( siMatchNumber,
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
checkCReduce ring bigno nLive live diffangle sameangle contract = return True



