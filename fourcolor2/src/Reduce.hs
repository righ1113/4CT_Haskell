{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Reduce ( reduce ) where

import CoLibCConst
    ( GConfMajor(..),
      TpUpdateState2,
      TpAnglePack,
      TpEdgeNo,
      TpConfFmt,
      TpLiveTwin,
      power,
      siMatchNumber,
      maxRing )

import ReLibEdgeNo2                   ( getEdgeNo )
import ReLibAngles                    ( findAngle )
import ReLibFindlive                  ( findLive )
import ReLibUpdateLive                ( testMatch )

import Data.Function                  ( fix )
import Debug.Trace                    ( trace )
import Data.Array                     ( (!), (//), array )



reduce :: String -> [Bool]
reduce gStr =
--  all (chkCReduce . chkDReduce . makeLive . makeAngle . makeEdgeNo . makeGConfMajor) . (read :: String -> [TpConfFmt])
  map (chkCReduce . chkDReduce . makeLive . makeAngle . makeEdgeNo . makeGConfMajor) gConfs where
    gConfs = take 12 (read gStr :: [TpConfFmt])


makeGConfMajor :: TpConfFmt -> (TpConfFmt, GConfMajor)
makeGConfMajor gConf = trace "major ok." (
  gConf,
  Major {
    verts  = verts0,
    ring   = ring0,
    term   = 3 * (verts0 - 1) - ring0,
    edges  = 3 * verts0 - 3 - ring0,
    claim  = gConf !! 1 !! 2,
    cont0  = gConf !! 2 !! 0,
    contE  = gConf !! 1 !! 3,
    bigno  = (power !! (ring0 + 1) - 1) `div` 2,
    ncodes = trace (show ring0) (power !! ring0       + 1) `div` 2,
    nchar  = (siMatchNumber !! ring0) `div` 8 + 1 } ) where
  verts0 = gConf !! 1 !! 0
  ring0  = gConf !! 1 !! 1


makeEdgeNo :: (TpConfFmt, GConfMajor) -> (TpConfFmt, GConfMajor, TpEdgeNo)
makeEdgeNo (gConf, m) = (gConf, m, getEdgeNo (verts m) (ring m) (term m) gConf)


makeAngle :: (TpConfFmt, GConfMajor, TpEdgeNo) -> (GConfMajor, TpAnglePack)
makeAngle (a, m, c) = (m, findAngle (a, c))


makeLive :: (GConfMajor, TpAnglePack) -> TpUpdateState2
makeLive (m, d@(_, _, an, _, _, _)) = (fl, real, 0, 1, 0, m, d, False, True) where
  live = array (0, ncodes m) [(i, 1) | i <- [0..(ncodes m)]]
  fl   = findLive (ring m) (bigno m) live (ncodes m) an (claim m)
  real = replicate (siMatchNumber !! maxRing `div` 8 + 2) 255


chkDReduce :: TpUpdateState2 -> TpUpdateState2
chkDReduce = until p (updateLive . testMatch) where
  p :: TpUpdateState2 -> Bool
  p (_, _, _, _, _, _, _, b1, _) = b1
updateLive :: TpUpdateState2 -> TpUpdateState2
updateLive (twin, real, nReal, _, _, m, d, _, _) = (twin2, real, nReal, 1, 0, m, d, b1, b2) where
  (b1, b2, twin2) = isUpdate (ncodes m) twin nReal
  isUpdate :: Int -> TpLiveTwin -> Int -> (Bool, Bool, TpLiveTwin)
  isUpdate nCodes (nLive, live) _nReal =
    let _s1             = "\n\n\n                  ***  D-reducible  ***\n"
        _s2             = "\n\n\n                ***  Not D-reducible  ***\n"
        liveB           = if live ! 0 > 1 then live // [(0, 15)] else live
        (nLive2, live2) = flip fix (0, liveB, 0) $ \loop (nLive', live', i) -> case () of
                            _ | i >= nCodes      -> (nLive', live')
                              | live' ! i /= 15  -> loop (nLive',  live'3, i + 1)
                              | otherwise        -> loop (nLive'2, live'2, i + 1) where
                                  nLive'2 = nLive' + 1
                                  live'2  = live' // [(i, 1)]
                                  live'3  = live' // [(i, 0)]
    --putStrLn $ "                       " ++ show nReal -- right
    --putStr $ "              " ++ show nLive2           -- left
    in case () of
      _ | 0 < nLive2 && nLive2 < nLive -> trace (show _nReal ++ " " ++ show nLive2) (False, False,       (nLive2, live2)) -- 続行
        | otherwise                    -> trace (show _nReal ++ " " ++ show nLive2) (True,  nLive2 == 0, (nLive2, live2)) -- 終了
            --if nLive2 == 0 then putStr s1 else putStr s2


chkCReduce :: TpUpdateState2 -> Bool
chkCReduce (_, _, _, _, _, _, _, _, b2) = b2
--chkCReduce (_, _, _, _, _, _, _, b1, _) = b1



