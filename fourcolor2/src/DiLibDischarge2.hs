module DiLibDischarge2( dischgCore ) where

import CoLibCConst
    ( TpReducePack,
      TpPosout,
      TpAxleI,
      TpAxle,
      maxoutlets,
      difNouts,
      getPosoutI, outletForced, outletPermitted )
-- import DiLibReduce               ( reduce )
-- import DiLibApply                ( getPosoutI, outletForced, outletPermitted )
--import Control.Applicative       ( empty )
import Control.Lens
    ( (&),
      (^.),
      (.~),
      Ixed(ix),
      Field2(_2),
      Field3(_3),
      Field4(_4),
      Field5(_5),
      Field6(_6) )
-- import Control.Monad.IO.Class    ( liftIO )
-- import Control.Monad.Trans.Class ( lift )
--import Control.Monad.Trans.Maybe ( MaybeT(..) )
-- import Control.Monad.Trans.RWS   ( RWST(..), ask, get, put )
--import Data.Maybe ( isNothing )
import Debug.Trace ( trace )
--import Text.Printf               ( printf )


{-
discharge()
  checkHubcap()
    dischargeCore()
      -- 1. compute forced and permitted rules, allowedch, forcedch, update s
        dischargeCoreSub1()|
      -- 2. print
      -- 3. check if inequality holds
      -- 4. check reducibility
        reduce()|
      -- 5.
        dischargeCoreSub5()
      -- 6. error

      dischargeCoreSub5()
        -- 5.1. accepting positioned outlet PO, computing AA
          dischargeCoreSub5Sub1()|
        -- 5.2. Check if a previously rejected positioned outlet is forced to apply
          dischargeCoreSub5Sub2()|
          dischargeCore()| 相互再帰
        -- 5.3. rejecting positioned outlet PO
        -- 5.4. recursion
-}

dischgCore :: [String] -> TpAxle -> TpPosout -> Int -> TpReducePack -> Int
--  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
dischgCore strs _ax@(axLow, axUpp, axLev) rules deg rP =
  -- 1. omitted
  -- 2. omitted
  -- 3. omitted
  -- 4. omitted
  -- 5.
  -- (_, _, deg)     <- ask
  -- (rP, _)         <- get
  let xyvList      = map read strs :: [(Int, Int, Int)]
      s            = replicate (2 * maxoutlets + 1) 0
      nouts        = difNouts !! deg
      ret = trace "    checkHubcap start." checkHubcap 0 (axLow !! axLev, axUpp !! axLev) s xyvList rP nouts rules deg
  in ret --return "checkHubcap end."


checkHubcap :: Int -> TpAxleI -> [Int] -> [(Int, Int, Int)] -> TpReducePack -> Int -> TpPosout -> Int -> Int
--  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
checkHubcap i axL s xyvList rP nouts rules deg
  | i >= length xyvList = deg --return "end checkHubcapSub"
  | otherwise           =
      --liftIO $ printf "\n-->Checking hubcap member %d, %d, %d\n" xi yi vi
      --put (rP, if xi /= yi then posX1 else posX2)
      --_ <- trace "    dischargeCore start1." $ return $ runMaybeT $ dischargeCore axL (s & ix (if xi /= yi then 2 * nouts else nouts) .~ 99) vi 0 0 rules posX deg
      checkHubcap (i + 1) axL s xyvList rP nouts rules deg + nl - 1 where
        nl = fromEnum $ dischargeCore axL (s & ix (if xi /= yi then 2 * nouts else nouts) .~ 99) vi 0 0 rules posX deg
        (xi, yi, vi) = xyvList !! i
        posX1        = replicate nouts xi ++ replicate nouts yi
        posX2        = replicate nouts xi ++ replicate nouts 0
        posX = if xi /= yi then posX1 else posX2


dischargeCore :: TpAxleI -> [Int] -> Int -> Int -> Int -> TpPosout -> [Int] -> Int -> Bool
dischargeCore axL s0 maxch pos depth rules posoutX deg =

  -- 1. compute forced and permitted rules, allowedch, forcedch, update s
  let (forcedch, allowedch, s) = dischargeCoreSub1 0 0 0 s0 axL rules posoutX deg

  -- 3. check if inequality holds
  in if forcedch + allowedch <= maxch then
    trace ("Inequality holds. Case done. " ++ show forcedch ++ " " ++ show allowedch ++ " " ++ show maxch) True -- true end
  else

    -- 4. check reducibility
    if forcedch > maxch then
      -- lift $ put (((aSLow & ix 0 .~ axLowL, aSUpp & ix 0 .~ axUppL, aSLev), used, image, adjmat, edgelist), posoutX)
      -- ret <- (lift . runMaybeT . reduce) 1
      -- if isNothing ret then
      --   error "Incorrect hubcap upper bound"
      -- else do
        --liftIO $ printf "%d, %d, %d Reducible. Case done.\n" forcedch allowedch maxch
      trace ("Reducible. Case done. " ++ show forcedch ++ " " ++ show allowedch ++ " " ++ show maxch) True -- true end
    else
      --liftIO $ printf ""

      -- 5.
      if dischargeCoreSub5 forcedch allowedch s maxch pos depth axL posoutX rules deg then
        True
      else
        -- 6. error
        error "Unexpected error 101"


dischargeCoreSub1 :: Int -> Int -> Int -> [Int] -> TpAxleI -> TpPosout -> [Int] -> Int -> (Int, Int, [Int])
dischargeCoreSub1 i forcedch allowedch retS axL rules posoutX deg
  | retS !! i >= 99 = (forcedch, allowedch, retS)
  | otherwise       = dischargeCoreSub1 (i + 1) forcedch3 allowedch2 retS2 axL rules posoutX deg where
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


dischargeCoreSub5 :: Int -> Int -> [Int] -> Int -> Int -> Int -> TpAxleI -> [Int] -> TpPosout -> Int -> Bool
--  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
dischargeCoreSub5 forcedch allowedch s maxch pos depth (axLowL, axUppL) posoutX rules deg
  | s !! pos >= 99                            = True --return "5. end."
  | s !! pos /= 0 || (rules ^. _3) !! pos < 0
    = dischargeCoreSub5 forcedch allowedch s maxch (pos + 1) depth (axLowL, axUppL) posoutX rules deg
  | otherwise =
      -- 5.1. accepting positioned outlet PO, computing AA
      let x                  = posoutX !! pos
          (axLowL2, axUppL2) = dischargeCoreSub5Sub1 0 x (axLowL, axUppL) rules pos deg

      -- 5.2. Check if a previously rejected positioned outlet is forced to apply
          good = dischargeCoreSub5Sub2 0 x depth s pos rules (axLowL2, axUppL2) posoutX deg
          foo = if good then
            --        return True --liftIO $ printf ""
            True
          else
            dischargeCore (axLowL2, axUppL2) (s & ix pos .~ 1) maxch (pos + 1) (depth + 1) rules posoutX deg
            --  return True

      -- 5.3. rejecting positioned outlet PO
          s2         = s  & ix pos .~ (-1)
          allowedch2 = allowedch - ((rules ^. _3) !! pos)
      in if not foo || allowedch2 + forcedch <= maxch then do
        --liftIO $ printf "Inequality holds.\n"
        trace ("        Inequality holds. " ++ show forcedch ++ " " ++ show allowedch2 ++ " " ++ show maxch) True -- true end
      else
        --return True --liftIO $ printf ""

        -- 5.4. recursion
        dischargeCoreSub5 forcedch allowedch2 s2 maxch (pos + 1) depth (axLowL, axUppL) posoutX rules deg


dischargeCoreSub5Sub1 :: Int -> Int -> TpAxleI -> TpPosout -> Int -> Int -> ([Int], [Int])
dischargeCoreSub5Sub1 i x (axLowL, axUppL) rules pos deg
  | i >= (rules ^. _2) !! pos   = (axLowL, axUppL)
  | axLowL2 !! p > axUppL2 !! p = error "Unexpected error 321"
  | otherwise                   = dischargeCoreSub5Sub1 (i + 1) x (axLowL2, axUppL2) rules pos deg where
      p0      = ((rules ^. _4) !! pos) !! i
      p       = if x - 1 + (p0 - 1) `mod` deg < deg then p0 + x - 1 else p0 + x - 1 - deg
      lowPosI = ((rules ^. _5) !! pos) !! i
      uppPosI = ((rules ^. _6) !! pos) !! i
      (axLowL2, axUppL2)
        | lowPosI >  axLowL !! p && uppPosI <  axUppL !! p = (axLowL & ix p .~ lowPosI, axUppL & ix p .~ uppPosI)
        | lowPosI <= axLowL !! p && uppPosI <  axUppL !! p = (axLowL                  , axUppL & ix p .~ uppPosI)
        | lowPosI >  axLowL !! p && uppPosI >= axUppL !! p = (axLowL & ix p .~ lowPosI, axUppL                  )
        | lowPosI <= axLowL !! p && uppPosI >= axUppL !! p = (axLowL                  , axUppL                  )
        | otherwise                                        = error "dischargeCoreSub5Sub1 error!"


dischargeCoreSub5Sub2 :: Int -> Int -> Int -> [Int] -> Int -> TpPosout -> TpAxleI -> [Int] -> Int -> Bool
dischargeCoreSub5Sub2 i x depth s pos rules (axLowL2, axUppL2) posoutX deg
  | i >= pos                  = False
  | s !! i == -1 && retF /= 0 =
      True -- 上位で相互再帰しない
  | otherwise                 = dischargeCoreSub5Sub2 (i + 1) x depth s pos rules (axLowL2, axUppL2) posoutX deg where
      retF = outletForced (axLowL2, axUppL2) (getPosoutI rules i) (posoutX !! i) deg



