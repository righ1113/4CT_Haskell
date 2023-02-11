{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use head" #-}
--{-# LANGUAGE Strict #-}
module ReLibUpdateLive( updateLive ) where

import CoLibCConst
    ( siMatchNumber,
      power,
      maxRing,
      debugLogUpdateLive,
      TpRealityPack,
      TpBaseCol,
      TpTMbind,
      TpUpdateState2,
      TpLiveTwin,
      TpRingNchar )
import Control.Applicative            ( empty )
import Control.Arrow                  ( (<<<) )
import Control.Lens                   ( (&), (.~), Ixed(ix), (%~) )
-- import Control.Monad.Trans.Class      ( lift )
-- import Control.Monad.Trans.Maybe      ( MaybeT(..) )
-- import Control.Monad.Trans.State.Lazy ( StateT(..), execStateT, get, put )
import Data.Bits                      ( Bits(shift, (.&.), (.|.), xor) )
import Data.Function                  ( fix )
-- import Data.Int                       ( Int8 )
import Data.Maybe                     ( isNothing, fromJust )


updateLive :: TpRingNchar -> Int -> TpLiveTwin -> IO TpLiveTwin
updateLive = iterateConvergenceIO testMatch real where
  real = replicate (siMatchNumber !! maxRing `div` 8 + 2) 255


iterateConvergenceIO :: (TpUpdateState2 -> TpUpdateState2) -> [Int] -> TpRingNchar -> Int -> TpLiveTwin -> IO TpLiveTwin
--iterateConvergenceIO f real rn nCodes lTwin = return lTwin
{--}
iterateConvergenceIO f real rn nCodes lTwin = do
  let (lTwin2, real2, nReal, _, _, _) = f (lTwin, real, 0, 1, 0, rn)
  (is, lTwin3) <- isUpdate nCodes lTwin2 nReal
  case () of
    _ | not is    -> return lTwin3
      | otherwise -> iterateConvergenceIO f real2 rn nCodes lTwin3
{--}


isUpdate :: Int -> TpLiveTwin -> Int -> IO (Bool, TpLiveTwin)
isUpdate nCodes (nLive, live) nReal = do
  let s1              = "\n\n\n                  ***  D-reducible  ***\n"
      s2              = "\n\n\n                ***  Not D-reducible  ***\n"
      live'           = if live !! 0 > 1 then live & ix 0 .~ 15 else live
      (nLive2, live2) = flip fix (0, live', 0) $ \loop (nLive1, live1, i) -> case () of
                          _ | i >= nCodes      -> (nLive1, live1)
                            | live1 !! i /= 15 -> loop (nLive1,   live3,   i + 1)
                            | otherwise        -> loop (nLive2_2, live2_2, i + 1) where
                                nLive2_2 = nLive1 + 1
                                live2_2  = live1 & ix i .~ 1
                                live3    = live1 & ix i .~ 0
  putStrLn $ "                       " ++ show nReal -- right
  putStr $ "              " ++ show nLive2           -- left
  case () of
    _ | 0 < nLive2 && nLive2 < nLive -> return (True, (nLive2, live2)) -- 続行
      | otherwise                    -> do
          if nLive2 == 0 then putStr s1 else putStr s2
          return (False, (nLive2, live2))                              -- 終了


-- ======== testmatch ========
testMatch :: TpUpdateState2 -> TpUpdateState2
testMatch st@(_, _, _, _, _, (ring, _)) = ret where
  (_, ret) = (testMatchSub2wrapAug False (ring, ring)
              . testMatchSub1 False
                . testMatchSub2wrapAug True (2, ring - 1)
                  . testMatchSub1 True) ((replicate 10 0, replicate 16 $ replicate 4 0, replicate 16 $ replicate 16 $ replicate 4 0), st)


testMatchSub1 :: Bool -> (TpTMbind, TpUpdateState2) -> (TpTMbind, TpUpdateState2)
testMatchSub1 flg ((interval, weight, matchW), st@(_, _, _, _, _, (ring, _))) = ((interval, weight, matchW2), st) where
  matchW2 = flip fix (matchW, 2) $ \loop1 (matchWin1, a) -> case () of
              _ | a > ring -> matchWin1
                | otherwise -> loop1 (matchW3, a + 1) where
                    matchW3 = flip fix (matchWin1, 1) $ \loop2 (matchWin2, b) -> case () of
                                _ | b > a - 1 -> matchWin2
                                  | otherwise -> loop2 (nextMatch, b + 1) where
                                      matchW4_1 = debugLogUpdateLive ("aM,bM: " ++ show a ++ " " ++ show b) $ matchWin2 & (ix a <<< ix b <<< ix 0) .~ (power !! a + power !! b) * 2
                                      matchW4_2 = matchW4_1 & (ix a <<< ix b <<< ix 1) .~ (power !! a - power !! b) * 2
                                      matchW4_3 = matchW4_2 & (ix a <<< ix b <<< ix 2) .~ (power !! a + power !! b)
                                      matchW4_4 = matchW4_3 & (ix a <<< ix b <<< ix 3) .~ (power !! a - power !! b)
                                      matchW5_1 = matchWin2 & (ix a <<< ix b <<< ix 0) .~ (power !! a + power !! b)
                                      matchW5_2 = matchW5_1 & (ix a <<< ix b <<< ix 1) .~ (power !! a - power !! b)
                                      matchW5_3 = matchW5_2 & (ix a <<< ix b <<< ix 2) .~ -power !! a - power !! b
                                      matchW5_4 = matchW5_3 & (ix a <<< ix b <<< ix 3) .~ -power !! a -(2 * power !! b)
                                      nextMatch = if flg then matchW4_4 else matchW5_4


testMatchSub2wrapAug :: Bool -> (Int, Int) -> (TpTMbind, TpUpdateState2) -> (TpTMbind, TpUpdateState2)
testMatchSub2wrapAug flg (start, end) ((interval0, weight0, matchW), st@(_, _, _, _, _, (ring, _))) =
  flip fix (interval0, weight0, start, st) $ \loop1 (interval, weight, a, st1) -> case () of
    _ | a > end   -> ((interval, weight, matchW), st1)
      | otherwise -> loop1 $
          flip fix (interval, weight, 1, st1) $ \loop (interval2, weight2, b, st2) -> case () of
                                        _ | b > a - 1 -> (interval2, weight2, a + 1, st2)
                                          | otherwise ->
                                              let
                                                weight4 = debugLogUpdateLive ("a,b: " ++ show a ++ " " ++ show b) $ weight2 & ix 1 .~ matchW !! a !! b
                                                n
                                                  | b >= 3 && a >= b + 3 = 2
                                                  | b >= 3 && a <  b + 3 = 1
                                                  | b <  3 && a >= b + 3 = 1
                                                  | otherwise            = 0
                                                interval4_1 = interval2   & ix 1           .~ 1
                                                interval4_2 = interval4_1 & ix 2           .~ b - 1
                                                interval4_3 = interval4_2 & ix (2 * n - 1) .~ b + 1
                                                interval4_4 = interval4_3 & ix (2 * n)     .~ a - 1
                                                interval5_3 = interval2   & ix (2 * n - 1) .~ b + 1
                                                interval5_4 = interval5_3 & ix (2 * n)     .~ a - 1
                                                interval4
                                                  | b >= 3 && a >= b + 3 = interval4_4
                                                  | b >= 3 && a <  b + 3 = interval4_2
                                                  | b <  3 && a >= b + 3 = interval5_4
                                                  | otherwise            = interval2
                                                baseCol = (power !! (ring + 1) - 1) `div` 2
                                                ((_, weight5, _), st3)
                                                  | flg       = augment0 (1, 0,       0) 1 n 0 ((interval4, weight4, matchW), st2)
                                                  | otherwise = augment0 (1, baseCol, 1) 1 n 0 ((interval4, weight4, matchW), st2)
                                              in loop (interval4, weight5, b + 1, st3)


-- ======== augment ========
augment0 :: TpBaseCol -> Int -> Int -> Int -> (TpTMbind, TpUpdateState2) -> (TpTMbind, TpUpdateState2)
augment0 bc@(depth, _, _) r n cnt (tm@(_, weight, _), st@(_, _, _, _, realTerm, (_, nchar))) = augment bc r n cnt (tm, ret) where
  ret = debugLogUpdateLive ("maxK, n: " ++ show (shift 1 (depth - 1)::Int)++ " " ++ show n ++ " ___ " ++ show realTerm ++ " " ++ show nchar ++ " ___ " ++ show depth)
    checkReality bc 0 weight (shift 1 (depth - 1)) (replicate 8 0) st


augment :: TpBaseCol -> Int -> Int -> Int -> (TpTMbind, TpUpdateState2) -> (TpTMbind, TpUpdateState2)
augment bc r n cnt pack@((interval, _, _), _)
  | cnt >= 10000 = error "augment over!"
  | r > n        = debugLogUpdateLive ("### r>n pass!! " ++ show r ++ " " ++ show n) pack
  | otherwise    = augment bc (r + 1) n cnt ret2 where
      lower = interval !! (2 * r - 1)
      upper = interval !! (2 * r)
      ret2  = if r > n then pack else augmentSub r (lower + 1) lower upper bc n cnt 0 pack


augmentSub :: Int -> Int -> Int -> Int -> TpBaseCol -> Int -> Int -> Int -> (TpTMbind, TpUpdateState2) -> (TpTMbind, TpUpdateState2)
augmentSub 1 i lower upper bc                      n cnt 0 pack = debugLogUpdateLive "### r=1 ###" augmentSub 1 i lower upper bc n cnt 1 pack
augmentSub r i lower upper bc@(depth, baseCol, on) n cnt _ pack@((interval, _, _), _) -- = pack
{--}
  = debugLogUpdateLive ("i, upper,  r, interval: " ++ show i ++ " " ++ show upper ++ "  " ++ show r ++ " " ++ show interval ++ " ___ " ++ show n) $ if i > upper then pack
    else
      let
        --lower          = interval !! (2 * r - 1)
        pack' =
          if i > upper then pack
          else flip fix (pack, n, lower) $ \loop (pack2@((va, we, ma), st), _, j) -> debugLogUpdateLive ("i,iover,j,jover: " ++ show i ++ " " ++ show upper ++ " " ++ show j ++ " " ++ show (i-1)) $ case () of
                _ | j >= i    -> pack2
                  | otherwise ->
                      let we2   = debugLogUpdateLive ("w: " ++ show i ++ " " ++ show j ++ " " ++ show (ma !! i !! j)) $ we & ix (depth + 1) .~ ma !! i !! j -- weight
                          bc'   = (depth + 1, baseCol, on)
                          newV  = take 10 $ take (2 * r - 2 + 1) va ++ replicate 100 0 -- take-cycle-take
                          newV2_1 = newV    & ix (2 * r - 1) .~ lower
                          newV2_2 = newV2_1 & ix (2 * r)     .~ j - 1
                          newV2_3 = newV2_2 & ix (2 * r + 1) .~ j + 1
                          newV2_4 = newV2_3 & ix (2 * r + 2) .~ i - 1
                          newV2_5 = newV    & ix (2 * r - 1) .~ j + 1
                          newV2_6 = newV2_5 & ix (2 * r)     .~ i - 1
                          (newN2, newV2)
                            | j >  lower + 1 && i >  j + 1 = (r + 1, newV2_4)
                            | j >  lower + 1 && i <= j + 1 = (r,     newV2_2)
                            | j <= lower + 1 && i >  j + 1 = (r,     newV2_6)
                            | otherwise                    = (r - 1, newV)
                          ((_, we3, ma3), st3) = if j >= i then pack else augment0 bc' 1 newN2 (cnt + 1) ((newV2, we2, ma), st)
                      in loop (((va, we3, ma3), st3), newN2, j + 1)
      in augmentSub r (i + 1) lower upper bc n cnt 1 pack'
{--}

-- ======== reality ========
checkReality :: TpBaseCol -> Int -> [[Int]] -> Int -> [Int] -> TpUpdateState2 -> TpUpdateState2
checkReality (16, _, _) _ _ _ _ _ = error "checkReality2 意図的なエラー!!"
checkReality bc@(depth, _, _) k weight maxK choice st@(lTwin, real, nReal, 0, realTerm, rn@(_, nchar))
  | k >= maxK = st
  | otherwise = if realTerm > nchar then error $ "More than %ld entries in real are needed " ++ show k ++ " " ++ show maxK ++ " " ++ show realTerm ++ " " ++ show nchar
                else debugLogUpdateLive ("^^^ " ++ show realTerm ++ " " ++ show depth) checkReality bc k weight maxK choice (lTwin, real, nReal, 1, realTerm + 1, rn)
checkReality bc@(depth, col, on) k weight maxK choice st@(lTwin, real, nReal, bit, realTerm, rn@(ring, _)) -- = st
{--}
  | k >= maxK                                  = st
  | fromIntegral bit .&. real !! realTerm == 0 =
      debugLogUpdateLive ("hogebit, realT, r(T): " ++ show bit ++ " " ++ show realTerm ++ " " ++ show (real !! realTerm)) checkReality bc (k + 1) weight maxK choice  (lTwin, real, nReal, shift bit 1, realTerm, rn) -- continue
  | otherwise                                  =
      let
        (parity2, choice2, col2) = debugLogUpdateLive ("bit, realT, r(T): " ++ show bit ++ " " ++ show realTerm ++ " " ++ show (real !! realTerm)) flip fix (ring .&. 1, choice, col, k, 1) $ \loop (parity, choice0, col0, left, i) -> case () of
                                    _ | i >= depth -> (parity, choice0, col0)
                                      | otherwise  -> loop (parity', choice', col', shift left (-1), i + 1) where
                                          (parity', choice', col')
                                            | left .&. 1 == 0 = debugLogUpdateLive ("leftU: " ++ show left) (parity,         choice0 & ix i .~ weight !! i !! 0, col0 + weight !! i !! 2)
                                            | otherwise       = debugLogUpdateLive ("leftD: " ++ show left) (parity `xor` 1, choice0 & ix i .~ weight !! i !! 1, col0 + weight !! i !! 3)
        (choice3, col3)
          | parity2 == 0                  = (choice2 & ix depth .~ weight !! depth !! 0, col2 + weight !! depth !! 2)
          | otherwise                     = (choice2 & ix depth .~ weight !! depth !! 1, col2 + weight !! depth !! 3)
        retM                     = debugLogUpdateLive ("col1,2,3,d,w: " ++ show col ++ " " ++ show col2 ++ " " ++ show col3 ++ " " ++ show depth ++ " " ++ show weight) $ isStillReal (depth, col3, on) choice3 lTwin
        (real2, nReal2, lTwin2)
          | isNothing retM                = (real & ix realTerm .~ real !! realTerm `xor` fromIntegral bit, nReal,     lTwin)
          | otherwise                     = (real,                                                          nReal + 1, fromJust retM)
      --in debugLogUpdateLive ("### lTwin2, nReal: " ++ show lTwin2 ++ " " ++ show nReal2 ++ " ___ " ++ show k ++ " " ++ show maxK ++ " " ++ show realTerm ++ " " ++ show nchar) checkReality (depth, col, on) (k + 1) weight maxK choice3 (lTwin2, real2, nReal2, shift bit 1, realTerm, rn)
      in checkReality (depth, col, on) (k + 1) weight maxK choice3 (lTwin2, real2, nReal2, shift bit 1, realTerm, rn)
{--}

isStillReal :: TpBaseCol -> [Int] -> TpLiveTwin -> Maybe TpLiveTwin
isStillReal (depth, col, on) choice lTwin = do
  pack                        <- stillRealSub1 col 0 lTwin (replicate 64 0, 0, replicate 64 0, replicate 64 0, 0)
  (twi2, nTw2, _, unt2, nUn2) <- flip fix (2, 1::Int, pack, 1) $ \loop (i, twoPow, packA, markA) -> case () of
                                  _ | i > depth -> return packA
                                    | otherwise -> do
                                        let c = choice !! i
                                        (pack2, mark2) <- flip fix (0, markA, packA) $ \loop2 (j, markB, packB@(_, _, sum0, _, _)) -> case () of
                                                            _ | j >= twoPow -> return (packB, markB)
                                                              | otherwise  -> do
                                                                  --let b = debugLogUpdateLive ("%%%%%% s_j, c: " ++ show (sum !! j) ++ " " ++ show c) sum !! j - c
                                                                  let b = sum0 !! j - c
                                                                  --pack3 <- debugLogUpdateLive ("twoPow: " ++ show twoPow) $ stillRealSub1 b mark lTwin pack
                                                                  pack3 <- stillRealSub1 b markB lTwin packB
                                                                  loop2 (j + 1, markB + 1, pack3)
                                        loop (i + 1, shift twoPow 1, pack2, mark2)
  let
    lTwin2
      | on == 0    = stillRealSub2 0 unt2 nUn2 2 $ stillRealSub2 0 twi2 nTw2 2 lTwin
      | otherwise  = stillRealSub2 0 unt2 nUn2 4 $ stillRealSub2 0 twi2 nTw2 8 lTwin
  return lTwin2


stillRealSub1 :: Int -> Int -> TpLiveTwin -> TpRealityPack -> Maybe TpRealityPack
stillRealSub1 b mark (_, live) rp@(twi, nTw, sum0, unt, nUn) = do -- return rp
{--}
  debugLogUpdateLive ("b: " ++ show b ++ " " ++ show rp) $ case () of
  --case () of
    _ | length live <= abs b        -> error (show (length live) ++ " " ++ show b ++ " stillRealSub1 意図的なエラー!!")
      | b <  0 && live !! (-b) == 0 -> empty
      | b <  0 && live !! (-b) /= 0 -> return (twi2, nTw2, sum2, unt,  nUn)
      | b >= 0 && live !! b    == 0 -> empty
      | otherwise                   -> return (twi,  nTw,  sum2, unt2, nUn2) where
          twi2
            | b < 0     = twi & ix nTw  .~ (-b)
            | otherwise = twi
          nTw2 = nTw + 1
          sum2 = sum0 & ix mark .~ b
          unt2
            | b < 0     = unt
            | otherwise = unt & ix nUn  .~ b
          nUn2 = nUn + 1
{--}

stillRealSub2 :: Int -> [Int] -> Int -> Int -> TpLiveTwin -> TpLiveTwin
stillRealSub2 i twist nTwist v lTwin@(nLive, live)
  | i >= nTwist = lTwin
  | otherwise   = stillRealSub2 (i + 1) twist nTwist v (nLive, live2) where
      index = twist !! i
      live2 = if i >= nTwist then live
              --else debugLogUpdateLive ("%%% i, t, nT,  index, value: " ++ show i ++ " " ++ show twist ++ " " ++ show nTwist ++ "  " ++ show (twist !! i) ++ " " ++ show (live !! (twist !! i) .|. v)) live & ix (twist !! i) .~ live !! (twist !! i) .|. v
              else live & ix index %~ (.|. v)



