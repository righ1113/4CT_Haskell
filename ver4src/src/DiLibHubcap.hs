module DiLibHubcap where

import DiLibCConst
    ( TpReducePack,
      TpPosout,
      TpGoodConf,
      TpAxleI,
      TpAxle,
      maxoutlets,
      difNouts )
import DiLibReduce               ( reduce )
import DiLibSymmetry             ( getPosoutI, outletForced, outletPermitted )
import Control.Applicative       ( empty )
import Control.Lens
    ( (&),
      (^.),
      (.~),
      Ixed(ix),
      Field1(_1),
      Field2(_2),
      Field3(_3),
      Field4(_4),
      Field5(_5),
      Field6(_6) )
import Control.Monad.IO.Class    ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.Trans.RWS   ( RWST(..), ask, get, put )
import Data.Maybe                ( isNothing )
import Text.Printf               ( printf )


{-
checkHubcap()
  checkHubcapSub()
    checkBound()
      -- 1. compute forced and permitted rules, allowedch, forcedch, update s
        checkBoundSub1()|
      -- 2. print
      -- 3. check if inequality holds
      -- 4. check reducibility
        reduce()|
      -- 5.
        checkBoundSub5()
      -- 6. error

      checkBoundSub5()
        -- 5.1. accepting positioned outlet PO, computing AA
          checkBoundSub5Sub1()|
        -- 5.2. Check if a previously rejected positioned outlet is forced to apply
          checkBoundSub5Sub2()|
          checkBound()| 相互再帰
        -- 5.3. rejecting positioned outlet PO
        -- 5.4. recursion
-}

checkHubcap :: [String] -> TpAxle
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
checkHubcap strs _ax@(axLow, axUpp, axLev) = do
  -- 1. omitted
  -- 2. omitted
  -- 3. omitted
  -- 4. omitted
  -- 5.
  (_, _, deg)     <- ask
  (rP, _)         <- get
  let xyvList      = map read strs :: [(Int, Int, Int)]
      s            = replicate (2 * maxoutlets + 1) 0
      nouts        = difNouts !! deg
  checkHubcapSub 0 (axLow !! axLev, axUpp !! axLev) s xyvList rP nouts
  return "checkHubcap end."


checkHubcapSub :: Int -> TpAxleI -> [Int] -> [(Int, Int, Int)] -> TpReducePack -> Int
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
checkHubcapSub i axL s xyvList rP nouts = ret
  where
    (xi, yi, vi) = xyvList !! i
    posX1        = replicate nouts xi ++ replicate nouts yi
    posX2        = replicate nouts xi ++ replicate nouts 0
    ret
      | i >= length xyvList = return "end checkHubcapSub"
      | otherwise           = do
          liftIO $ printf "\n-->Checking hubcap member %d, %d, %d\n" xi yi vi
          put (rP, if xi /= yi then posX1 else posX2)
          runMaybeT $ checkBound axL (s & ix (if xi /= yi then 2 * nouts else nouts) .~ 99) vi 0 0
          checkHubcapSub (i + 1) axL s xyvList rP nouts


checkBound :: TpAxleI -> [Int] -> Int -> Int -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
checkBound axL@(axLowL, axUppL) s0 maxch pos depth = do
  (_, rules, deg)                                                   <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- lift get

  -- 1. compute forced and permitted rules, allowedch, forcedch, update s
  let (forcedch, allowedch, s) = checkBoundSub1 0 0 0 s0 axL rules posoutX deg
  liftIO $ printf "f, a = %d, %d\n" forcedch allowedch

  -- 2. print
  liftIO $ printf "%d POs: " depth
  liftIO $ checkBoundSub2 0 s rules posoutX

  -- 3. check if inequality holds
  if forcedch + allowedch <= maxch then do
    liftIO $ printf "%d Inequality holds. Case done.\n" depth
    empty -- true end
  else
    liftIO $ printf ""

  -- 4. check reducibility
  if forcedch > maxch then do
    lift $ put (((aSLow & ix 0 .~ axLowL, aSUpp & ix 0 .~ axUppL, aSLev), used, image, adjmat, edgelist), posoutX)
    ret <- lift $ runMaybeT reduce
    if isNothing ret then
      error "Incorrect hubcap upper bound"
    else do
      liftIO $ printf "%d, %d, %d Reducible. Case done.\n" forcedch allowedch maxch
      empty -- true end
  else
    liftIO $ printf ""

  -- 5.
  checkBoundSub5 forcedch allowedch s maxch pos depth axL posoutX rules deg

  -- 6. error
  error "Unexpected error 101"


checkBoundSub1 :: Int -> Int -> Int -> [Int] -> TpAxleI -> TpPosout -> [Int] -> Int -> (Int, Int, [Int])
checkBoundSub1 i forcedch allowedch retS axL rules posoutX deg = ret
  where
    rVi       = (rules ^. _3) !! i
    forcedch2 = if retS !! i > 0 then forcedch + rVi else forcedch
    retF      = outletForced    axL (getPosoutI rules i) (posoutX !! i) deg
    retP      = outletPermitted axL (getPosoutI rules i) (posoutX !! i) deg
    (forcedch3, allowedch2, retS2)
      | retS !! i /= 0 = (forcedch2,       allowedch,       retS)
      | retF /= 0      = (forcedch2 + rVi, allowedch,       retS & ix i .~ 1)
      | retP == 0      = (forcedch2,       allowedch,       retS & ix i .~ (-1))
      | rVi > 0        = (forcedch2,       allowedch + rVi, retS)
      | otherwise      = (forcedch2,       allowedch,       retS)
    ret
      | retS !! i >= 99 = (forcedch, allowedch, retS)
      | otherwise       =
          checkBoundSub1 (i + 1) forcedch3 allowedch2 retS2 axL rules posoutX deg


checkBoundSub2 :: Int -> [Int] -> TpPosout -> [Int] -> IO ()
checkBoundSub2 i s rules posoutX
  | s !! i >= 99 = printf "\n"
  | s !! i < 0   = checkBoundSub2 (i + 1) s rules posoutX
  | otherwise    = do
      printf "%s" $ if s !! i == 0 then "?" else ""
      printf "%d, %d " ((rules ^. _1) !! i) (posoutX !! i)
      checkBoundSub2 (i + 1) s rules posoutX


checkBoundSub5 :: Int -> Int -> [Int] -> Int -> Int -> Int -> TpAxleI -> [Int] -> TpPosout -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
checkBoundSub5 forcedch allowedch s maxch pos depth (axLowL, axUppL) posoutX rules deg
  | s !! pos >= 99                            = return "5. end."
  | s !! pos /= 0 || (rules ^. _3) !! pos < 0
    = checkBoundSub5 forcedch allowedch s maxch (pos + 1) depth (axLowL, axUppL) posoutX rules deg
  | otherwise = do
      -- 5.1. accepting positioned outlet PO, computing AA
      let x                  = posoutX !! pos
          (axLowL2, axUppL2) = checkBoundSub5Sub1 0 x (axLowL, axUppL) rules pos deg

      -- 5.2. Check if a previously rejected positioned outlet is forced to apply
      good <- liftIO $ checkBoundSub5Sub2 0 x depth s pos rules (axLowL2, axUppL2) posoutX deg
      if good then
        liftIO $ printf ""
      else do
        liftIO $ printf "%d Starting recursion with \n" depth
        liftIO $ printf "%d, %d forced\n" ((rules ^. _1) !! pos) x
        lift . runMaybeT $ checkBound (axLowL2, axUppL2) (s & ix pos .~ 1) maxch (pos + 1) (depth + 1)
        liftIO $ printf ""

      -- 5.3. rejecting positioned outlet PO
      liftIO $ printf "%d Rejecting positioned outlet \n" depth
      liftIO $ printf "%d, %d\n" ((rules ^. _1) !! pos) x
      let s2         = s  & ix pos .~ (-1)
          allowedch2 = allowedch - ((rules ^. _3) !! pos)
      if allowedch2 + forcedch <= maxch then do
        liftIO $ printf "Inequality holds.\n"
        empty -- true end
      else
        liftIO $ printf ""
      
      -- 5.4. recursion
      checkBoundSub5 forcedch allowedch2 s2 maxch (pos + 1) depth (axLowL, axUppL) posoutX rules deg


checkBoundSub5Sub1 :: Int -> Int -> TpAxleI -> TpPosout -> Int -> Int -> ([Int], [Int])
checkBoundSub5Sub1 i x (axLowL, axUppL) rules pos deg = ret
  where
    p0      = ((rules ^. _4) !! pos) !! i
    p       = if x - 1 + (p0 - 1) `mod` deg < deg then p0 + x - 1 else p0 + x - 1 - deg
    lowPosI = ((rules ^. _5) !! pos) !! i
    uppPosI = ((rules ^. _6) !! pos) !! i
    (axLowL2, axUppL2)
      | lowPosI >  axLowL !! p && uppPosI <  axUppL !! p = (axLowL & ix p .~ lowPosI, axUppL & ix p .~ uppPosI)
      | lowPosI <= axLowL !! p && uppPosI <  axUppL !! p = (axLowL                  , axUppL & ix p .~ uppPosI)
      | lowPosI >  axLowL !! p && uppPosI >= axUppL !! p = (axLowL & ix p .~ lowPosI, axUppL                  )
      | lowPosI <= axLowL !! p && uppPosI >= axUppL !! p = (axLowL                  , axUppL                  )
    ret
      | i >= (rules ^. _2) !! pos   = (axLowL, axUppL)
      | axLowL2 !! p > axUppL2 !! p = error "Unexpected error 321"
      | otherwise                   =
          checkBoundSub5Sub1 (i + 1) x (axLowL2, axUppL2) rules pos deg


checkBoundSub5Sub2 :: Int -> Int -> Int -> [Int] -> Int -> TpPosout -> TpAxleI -> [Int] -> Int -> IO Bool
checkBoundSub5Sub2 i x depth s pos rules (axLowL2, axUppL2) posoutX deg = ret
  where
    retF = outletForced (axLowL2, axUppL2) (getPosoutI rules i) (posoutX !! i) deg
    ret
      | i >= pos                  = return False
      | s !! i == -1 && retF /= 0 = do
          printf "%d Positioned outlet " depth
          printf "%d, %d can't be forced, because it forces %d, %d\n" ((rules ^. _1) !! pos) x ((rules ^. _1) !! i) (posoutX !! i)
          return True -- 上位で相互再帰しない
      | otherwise                 =
          checkBoundSub5Sub2 (i + 1) x depth s pos rules (axLowL2, axUppL2) posoutX deg



