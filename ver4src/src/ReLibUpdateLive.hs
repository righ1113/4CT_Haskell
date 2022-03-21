{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module ReLibUpdateLive where

import CoLibCConst                    ( TpLiveTwin, TpUpdateState, TpRingNchar, TpBaseCol, TpTMbind, TpRealityPack, maxring, power, simatchnumber)
import Control.Applicative            ( empty )
import Control.Arrow                  ( (<<<) )
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
    ((nLive, live), real2, _, _, _) <- execStateT (testmatch ring nchar) now
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
testmatch :: Int -> Int -> StateT TpUpdateState IO ()
testmatch ring nchar =
  flip (>>) testmatchSub5
    $ testmatchSub2wrapAug (ring, nchar) False (ring, ring)
      =<< testmatchSub1 ring False
        =<< testmatchSub2wrapAug (ring, nchar) True (2, ring - 1)
          =<< testmatchSub1 ring True (replicate 10 0, replicate 16 $ replicate 4 0, replicate 16 $ replicate 16 $ replicate 4 0)


testmatchSub1 :: Int -> Bool -> TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub1 ring flg (interval, weight, matchW) = return (interval, weight, matchW2) where
  matchW2 = flip fix (matchW, 2) $ \loop (matchW, a) -> case () of
              _ | a > ring -> matchW
                | otherwise -> loop (matchW3, a + 1) where
                    matchW3 = flip fix (matchW, 1) $ \loop (matchW, b) -> case () of
                                _ | b > a - 1 -> matchW
                                  | otherwise -> loop (nextMatch, b + 1) where
                                      matchW4_1 = matchW    & (ix a <<< ix b <<< ix 0) .~ (power !! a + power !! b) * 2
                                      matchW4_2 = matchW4_1 & (ix a <<< ix b <<< ix 1) .~ (power !! a - power !! b) * 2
                                      matchW4_3 = matchW4_2 & (ix a <<< ix b <<< ix 2) .~ (power !! a + power !! b)
                                      matchW4_4 = matchW4_3 & (ix a <<< ix b <<< ix 3) .~ (power !! a - power !! b)
                                      matchW5_1 = matchW    & (ix a <<< ix b <<< ix 0) .~ (power !! a + power !! b)
                                      matchW5_2 = matchW5_1 & (ix a <<< ix b <<< ix 1) .~ (power !! a - power !! b)
                                      matchW5_3 = matchW5_2 & (ix a <<< ix b <<< ix 2) .~ -power !! a + power !! b
                                      matchW5_4 = matchW5_3 & (ix a <<< ix b <<< ix 3) .~ -power !! a -2 * power !! b
                                      nextMatch = if flg then matchW4_4 else matchW5_4


testmatchSub2wrapAug :: TpRingNchar -> Bool -> (Int, Int) -> TpTMbind -> StateT TpUpdateState IO TpTMbind
testmatchSub2wrapAug (ring, nchar) flg (start, end) (interval, weight, matchW) =
  flip fix (interval, weight, start) $ \loop (interval, weight, a) -> case () of
    _ | a > end   -> return (interval, weight, matchW)
      | otherwise ->
          (flip fix (interval, weight, 1) $ \loop (interval, weight, b) -> case () of
                                        _ | b > a - 1 -> return (interval, weight, a + 1)
                                          | otherwise -> do
                                              let
                                                weight4 = weight & ix 1 .~ matchW !! a !! b
                                                n
                                                  | b >= 3 && a >= b + 3 = 2
                                                  | b >= 3 && a <  b + 3 = 1
                                                  | b <  3 && a >= b + 3 = 1
                                                  | otherwise            = 0
                                                interval4_1 = interval    & ix 1           .~ 1
                                                interval4_2 = interval4_1 & ix 2           .~ b - 1
                                                interval4_3 = interval4_2 & ix (2 * n - 1) .~ b + 1
                                                interval4_4 = interval4_3 & ix (2 * n)     .~ a - 1
                                                interval5_3 = interval    & ix (2 * n - 1) .~ b + 1
                                                interval5_4 = interval5_3 & ix (2 * n)     .~ a - 1
                                                interval4
                                                  | b >= 3 && a >= b + 3 = interval4_4
                                                  | b >= 3 && a <  b + 3 = interval4_2
                                                  | b <  3 && a >= b + 3 = interval5_4
                                                  | otherwise            = interval
                                                baseCol = (power !! (ring + 1) - 1) `div` 2
                                              if flg
                                                then augment (ring, nchar) (1, 0,       0) n 0 (interval4, weight4, matchW)
                                                else augment (ring, nchar) (1, baseCol, 1) n 0 (interval4, weight4, matchW)
                                              loop (interval4, weight4, b + 1))
          >>= loop


testmatchSub5 :: StateT TpUpdateState IO ()
testmatchSub5 = do
  (_, _, nreal, _, _) <- get
  lift $ putStrLn $ "                       " ++ show nreal -- right


-- ======== augment ========
augment :: TpRingNchar -> TpBaseCol -> Int -> Int -> TpTMbind -> StateT TpUpdateState IO TpTMbind
augment rn bc                      n 10000 tm = error "augment over!"
augment rn bc@(depth, basecol, on) n cnt   tm@(interval, weight, matchW) = do
  checkReality rn bc 0 weight (shift 1 (depth - 1)) (replicate 8 0)
  flip fix 1 $ \loop r -> case () of
    _ | r > n     -> return tm
      | otherwise -> do
          let lower = interval !! (2 * r - 1)
          tm' <- augmentSub r (lower + 1) rn bc n cnt tm
          loop (r + 1)


augmentSub :: Int -> Int -> TpRingNchar -> TpBaseCol -> Int -> Int -> TpTMbind -> StateT TpUpdateState IO TpTMbind
augmentSub r i rn bc@(depth, basecol, on) n cnt tm@(interval, weight, matchW)
  | i > upper = return tm
  | otherwise = do
      (tm', newN') <- flip fix (tm, n, lower) $ \loop (tm@(va, we, ma), newN, j) -> case () of
                        _ | j > i     -> return (tm, newN)
                          | otherwise -> do
                              let we2   = we & ix (depth + 1) .~ ma !! i !! j       -- weight
                                  bc'   = (depth + 1, basecol, on)
                                  newV  = take 10 $ take (2 * r - 2) va ++ repeat 0 -- take-cycle-take
                                  newV2_1 = newV    & ix (2 * r - 1) .~ lower
                                  newV2_2 = newV2_1 & ix (2 * r)     .~ j - 1
                                  newV2_3 = newV2_2 & ix (2 * r + 1) .~ j + 1
                                  newV2_4 = newV2_2 & ix (2 * r + 2) .~ i - 1
                                  newV2_5 = newV    & ix (2 * r - 1) .~ j + 1
                                  newV2_6 = newV2_5 & ix (2 * r)     .~ i - 1
                                  (newN2, newV2)
                                    | j >  lower + 1 && i >  j + 1 = (r + 1, newV2_4)
                                    | j >  lower + 1 && i <= j + 1 = (r,     newV2_2)
                                    | j <= lower + 1 && i >  j + 1 = (r,     newV2_6)
                                    | otherwise                    = (r - 1, newV)
                              tm'' <- augment rn bc' newN2 (cnt + 1) (newV2, we2, ma)
                              loop (tm'', newN2, j + 1)
      augmentSub r (i + 1) rn bc n cnt tm' where
        lower = interval !! (2 * r - 1)
        upper = interval !! (2 * r)


-- ======== reality ========
checkReality :: TpRingNchar -> TpBaseCol -> Int -> [[Int]] -> Int -> [Int] -> StateT TpUpdateState IO ()
checkReality rn@(ring, nchar) bc@(depth, col, on) k weight maxK choice = do
  (twin, real, nreal, bit, realterm) <- get
  -- if bit.zero? ...
  if bit == 0 then
    if realterm <= nchar then
      put (twin, real, nreal, 1, realterm + 1)
    else
      error "More than %ld entries in real are needed"
  else
    put (twin, real, nreal, bit, realterm)
  (_, _, _, bit2, realterm2) <- get
  case () of
    _ | k >= maxK                                  -> return ()
      | fromIntegral bit .&. real !! realterm == 0 -> do
          put (twin, real, nreal, shift bit2 1, realterm2)
          checkReality rn bc (k + 1) weight maxK choice
      | otherwise -> do
          (min, max) <- flip fix (4, 1) $ \loop (k, i) -> case () of
                          _ | i > depth -> return (k, i)
                            | otherwise -> do
                                loop (k, i + 1)
          retM <- runMaybeT $ isStillReal bc choice
          --case () of
          --  _ | isNothing retM -> undefined
          --    | otherwise      -> undefined
          checkReality rn bc (k + 1) weight maxK choice
{-
      (bit_lshift(pbit); next) if (pbit[0] & real[prealterm[0]]).zero?
      col, parity, left = basecol, ring & 1, k
      (1..(depth - 1)).each do |i|
        if (left & 1) != 0	# i.e. if a_i=1, where k=a_1+2a_2+4a_3+... */
          parity ^= 1 # XOR
          choice[i] = weight[i][1]
          col += weight[i][3]
        else
          choice[i] = weight[i][0]
          col += weight[i][2]
        end
        left >>= 1
      end
      if parity != 0
        choice[depth] = weight[depth][1]
        col += weight[depth][3]
      else
        choice[depth] = weight[depth][0]
        col += weight[depth][2]
      end
      if still_real?(col, choice, depth, on, live)
        pnreal[0] += 1
      else
        real[prealterm[0]] ^= pbit[0]
      end
      bit_lshift(pbit)
-}


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



