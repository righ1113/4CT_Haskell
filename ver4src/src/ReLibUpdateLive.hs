module ReLibUpdateLive where

import CoLibCConst                    ( TpLiveTwin, TpUpdateState, TpRingNchar, TpBaseCol, TpTMbind, TpRealityPack, simatchnumber, maxring )
import Control.Applicative            ( empty )
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Control.Monad.Trans.Class      ( lift )
import Control.Monad.Trans.Maybe      ( MaybeT(..) )
import Control.Monad.Trans.State.Lazy ( StateT(..), execStateT, get, put )
import Data.Bits                      ( Bits(shift, (.&.), (.|.)) )    
import Data.Function                  ( fix )
import Data.Int                       ( Int8 )
import Data.Maybe                     ( isNothing )


updateLive :: Int -> Int -> Int -> TpLiveTwin -> IO TpLiveTwin
updateLive ring nchar ncodes lTwin =
  flip fix (lTwin, real, 0, 1, 0) $ \loop now -> do
    ((nLive, live), real2, _, _, _) <- execStateT testmatch now
    (is, (nLive2, live2))           <- isUpdate ncodes (nLive, live)
    case () of
      _ | not is    -> return (nLive2, live2)
        | otherwise -> loop  ((nLive2, live2), real2, 0, 1, 0)
  where real = replicate (simatchnumber !! maxring `div` 8 + 2) 255


isUpdate :: Int -> TpLiveTwin -> IO (Bool, TpLiveTwin)
isUpdate ncodes (nLive, live) = do
  let s1              = "\n\n\n                  ***  D-reducible  ***\n"
      s2              = "\n\n\n                ***  Not D-reducible  ***\n"
      live'           = if head live > 1 then live & ix 0 .~ 15 else live
      (nLive2, live2) = flip fix (nLive, live', 0) $ \loop (nLive, live, i) -> case () of
                          _ | i >= ncodes     -> (nLive, live)
                            | live !! i /= 15 -> loop (nLive , live3, i + 1)
                            | otherwise       -> loop (nLive2, live2, i + 1) where
                                nLive2 = nLive + 1
                                live2  = live & ix i .~ 1
                                live3  = live & ix i .~ 0
  putStr $ "              " ++ show nLive2 -- left
  case () of
    _ | 0 < nLive2 && nLive2 < nLive -> return (True, (nLive2, live2)) -- 続行
      | otherwise                    -> do
          if nLive2 == 0 then putStr s1 else putStr s2
          return (False, (nLive2, live2)) -- 終了


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
  checkReality rn bc 0 [[]]
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


-- ======== reality ========
checkReality :: TpRingNchar -> TpBaseCol -> Int -> [[Int]] -> StateT TpUpdateState IO ()
checkReality rn bc@(depth, col, on) k weight = do
  let max    = shift 1 (depth - 1) :: Int
      choice = replicate 8 0
  (twin, real, nreal, bit, realterm) <- get
  -- if bit.zero? ...
  case () of
    _ | k >= max  -> return ()
      | False     -> do
          put (twin, real, nreal, shift bit 1, realterm)
          checkReality rn bc (k + 1) weight
      | otherwise -> do
          (min, max) <- flip fix (4, 1) $ \loop (k, i) -> case () of
                  _ | i > depth -> return (k, i)
                    | otherwise -> do
                        loop (k, i + 1)
          retM <- runMaybeT $ isStillReal bc choice
          case () of
            _ | isNothing retM -> undefined
              | otherwise      -> undefined
          checkReality rn bc (k + 1) weight


isStillReal :: TpBaseCol -> [Int] -> MaybeT (StateT TpUpdateState IO) ()
isStillReal bc choice = do
  case () of
    _ | True      -> empty     -- ここで終了
      | otherwise -> return () -- 次へ進む
  case () of
    _ | True      -> empty     -- ここで終了
      | otherwise -> return () -- 次へ進む
  return () -- 末尾なので、ここで終了


stillRealSub :: Int -> Int -> TpRealityPack -> MaybeT (StateT TpUpdateState IO) TpRealityPack
stillRealSub b mark (twi, nTw, sum, unt, nUn) = do
  ((_, live), _, _, _, _) <- lift get
  case () of
    _ | b <  0 && live !! (-b) == 0 -> empty
      | b <  0 && live !! (-b) /= 0 -> return (twi2, nTw2, sum2, unt,  nUn)
      | b >= 0 && live !! b    == 0 -> empty
      | otherwise                   -> return (twi,  nTw,  sum2, unt2, nUn2) where
          twi2 = twi & ix nTw .~ (-b)
          nTw2 = nTw + 1
          sum2 = sum & ix mark .~ b
          unt2 = unt & ix nUn .~ b
          nUn2 = nUn + 1



