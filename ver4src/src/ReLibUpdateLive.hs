module ReLibUpdateLive where

import CoLibCConst                    ( TpLiveTwin, TpUpdateState, TpRingNchar, TpBaseCol, TpTMbind, simatchnumber, maxring )
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Control.Monad.Trans.State.Lazy ( StateT(..), execStateT )
import Data.Bits                      ( Bits(shift, (.&.), (.|.)) )    
import Data.Function                  ( fix )
import Data.Int                       ( Int8 )


updateLive :: Int -> Int -> Int -> TpLiveTwin -> IO TpLiveTwin
updateLive ring nchar ncodes lTwin =
  flip fix (lTwin, real, 0, 1, 0) $ \loop now -> do
    next@((nlive, live), _, _, _, _) <- execStateT testmatch now
    is                               <- isUpdate
    case () of
      _ | not is    -> return (nlive, live)
        | otherwise -> loop next where
            c2 = 1
  where real = replicate (simatchnumber !! maxring `div` 8 + 2) 255


isUpdate = return False


-- ======== testmatch ========
testmatch :: StateT TpUpdateState IO ()
testmatch =
  flip (>>) testmatchSub5
    $ testmatchSub4wrapAug
      =<< testmatchSub3
        =<< testmatchSub2wrapAug
          =<< testmatchSub1 ([], [[]], [[[]]])


testmatchSub1 :: TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub1 = return


testmatchSub2wrapAug :: TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub2wrapAug = return


testmatchSub3 :: TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub3 = return


testmatchSub4wrapAug :: TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub4wrapAug = return


testmatchSub5 :: StateT TpUpdateState IO ()
testmatchSub5 = return ()


-- ======== augment ========
augment :: TpRingNchar -> TpBaseCol -> Int -> TpTMbind -> StateT TpUpdateState IO TpTMbind
augment rn bc n tm = do
  let lower = 2
  checkReality rn bc [[]]
  flip fix (4, lower) $ \loop (k, r) -> case () of
    _ | r > n     -> return tm
      | otherwise -> do
          tm' <- augmentSub r (lower + 1) rn bc n tm
          loop (k, r + 1)


augmentSub :: Int -> Int -> TpRingNchar -> TpBaseCol -> Int -> TpTMbind -> StateT TpUpdateState IO TpTMbind
augmentSub r i rn bc n tm
  | i > upper = return tm
  | otherwise = do
      (min, max) <- flip fix (4, lower) $ \loop (k, j) -> case () of
                      _ | j > i     -> return (k, j)
                        | otherwise -> do
                            -- weight
                            -- take-cycle-take
                            tm'' <- augment rn bc' newN tm'
                            loop (k, j + 1)
      augmentSub r (i + 1) rn bc' newN tm' where
        lower = 2
        upper = 4
        bc' = bc
        newN = n
        tm' = tm


checkReality :: TpRingNchar -> TpBaseCol -> [[Int]] -> StateT TpUpdateState IO ()
checkReality _ bc weight = do
  is <- isStillReal bc [] 
  case () of
    _ | not is    -> undefined
      | otherwise -> undefined


isStillReal :: TpBaseCol -> [Int] -> StateT TpUpdateState IO Bool
isStillReal = undefined



