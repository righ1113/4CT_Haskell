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


checkHubcap :: [String] -> TpAxle
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
checkHubcap strs _ax@(axLow, axUpp, axLev) = do
  -- 1. omitted
  -- 2. omitted
  -- 3. omitted
  -- 4. omitted
  -- 5.
  (_, _, deg)     <- ask
  (rP, _)   <- get
  let xyvList      = map read strs :: [(Int, Int, Int)]
      s            = replicate (2 * maxoutlets + 1) 0
      nouts        = difNouts !! deg
      loop1 i      =
        if i >= length xyvList then return "1. end."
        else do
          let (xi, yi, vi) = xyvList !! i
          liftIO . putStrLn $ "\n-->Checking hubcap member " ++ show (xyvList !! i)
          if xi /= yi then do
            let posX = replicate nouts xi ++ replicate nouts yi
            put (rP, posX)
            runMaybeT $ checkBound (axLow !! axLev, axUpp !! axLev) (s & ix (2 * nouts) .~ 99) vi 0 0
            -- liftIO $ print ret
          else do
            let posX = replicate nouts xi ++ replicate nouts 0
            put (rP, posX)
            runMaybeT $ checkBound (axLow !! axLev, axUpp !! axLev) (s & ix      nouts  .~ 99) vi 0 0
            -- liftIO $ print ret
          loop1 (i + 1)
  loop1 0
  return "checkHubcap end."


checkBound :: TpAxleI -> [Int] -> Int -> Int -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
checkBound axL@(axLowL, axUppL) s0 maxch _pos depth = do
  (_, rules, deg)                                              <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- lift get

  -- 1. compute forced and permitted rules, allowedch, forcedch, update s
  let loop1 i forcedch allowedch retS =
        if retS !! i >= 99 then
          (forcedch, allowedch, retS)
        else
          let rVi       = (rules ^. _3) !! i
              forcedch2 = if retS !! i > 0 then forcedch + rVi else forcedch
              retF      = outletForced    axL (getPosoutI rules i) (posoutX !! i)
              retP      = outletPermitted axL (getPosoutI rules i) (posoutX !! i)
              (forcedch3, allowedch2, retS2)
                | retS !! i /= 0 = (forcedch2,       allowedch,       retS)
                | retF /= 0      = (forcedch2 + rVi, allowedch,       retS & ix i .~ 1)
                | retP == 0      = (forcedch2,       allowedch,       retS & ix i .~ (-1))
                | rVi > 0        = (forcedch2,       allowedch + rVi, retS)
                | otherwise      = (forcedch2,       allowedch,       retS)
          in loop1 (i + 1) forcedch3 allowedch2 retS2
      (forcedch, allowedch, s) = loop1 0 0 0 s0
  -- (liftIO . print) axL
  liftIO . putStrLn $ "f, a = " ++ show forcedch ++ ", " ++ show allowedch

  -- 2.
  liftIO . putStr $ show depth ++ " POs: "
  let loop2 i
        | s !! i >= 99 = return ()
        | s !! i < 0   = loop2 (i + 1)
        | otherwise    = do
            putStr $ if s !! i == 0 then "?" else ""
            putStr $ show ((rules ^. _1) !! i) ++ "," ++ show (posoutX !! i) ++ " "
            loop2 (i + 1)
  liftIO $ loop2 0
  liftIO $ putStrLn ""

  -- 3. check if inequality holds
  if forcedch + allowedch <= maxch then do
    liftIO . putStrLn $ show depth ++ " Inequality holds. Case done."
    empty -- 正常終了
  else
    liftIO $ putStr ""

  -- 4. check reducibility
  if forcedch > maxch then do
    lift $ put (((aSLow & ix 0 .~ axLowL, aSUpp & ix 0 .~ axUppL, aSLev), used, image, adjmat, edgelist), posoutX)
    ret <- lift $ runMaybeT reduce
    if isNothing ret then
      error "Incorrect hubcap upper bound"
    else do
      liftIO . putStrLn $ show forcedch ++ " " ++ show allowedch ++ " " ++ show maxch
                  ++ " Reducible. Case done."
      empty -- 正常終了
  else
    liftIO $ putStr ""

  -- 5.
  let loop5 pos allowedch s --型がMaybeT
        | s !! pos >= 99                            = return "5. end."
        | s !! pos /= 0 || (rules ^. _3) !! pos < 0 = loop5 (pos + 1) allowedch s
        | otherwise = do
          let x                       = posoutX !! pos

          -- 5.1. accepting positioned outlet PO, computing AA
          let loop5_1 i axLowL axUppL =
                if i >= (rules ^. _2) !! pos then
                  (axLowL, axUppL)
                else
                  let p0      = ((rules ^. _4) !! pos) !! i
                      p       = if (x - 1 + (p0 - 1)) `mod` deg < deg then p0 + x - 1 else p0 + x - 1 - deg
                      lowPosI = ((rules ^. _5) !! pos) !! i
                      uppPosI = ((rules ^. _6) !! pos) !! i
                      (axLowL2, axUppL2)
                        |      lowPosI > axLowL !! p  &&      uppPosI < axUppL !! p
                            = (axLowL & ix p .~ lowPosI, axUppL & ix p .~ uppPosI)
                        | (lowPosI <= (axLowL !! p)) &&      uppPosI < axUppL !! p
                            = (axLowL                  , axUppL & ix p .~ uppPosI)
                        |      lowPosI > axLowL !! p  && (uppPosI >= (axUppL !! p))
                            = (axLowL & ix p .~ lowPosI, axUppL                  )
                        | (lowPosI <= (axLowL !! p)) && (uppPosI >= (axUppL !! p))
                            = (axLowL                  , axUppL                  )
                  in if axLowL2 !! p > axUppL2 !! p then
                    error "Unexpected error 321"
                  else
                    loop5_1 (i + 1) axLowL2 axUppL2
              (axLowL2, axUppL2)      = loop5_1 0 axLowL axUppL

          -- 5.2. Check if a previously rejected positioned outlet is forced to apply
          let loop5_2 i =
                if i >= pos then
                  return 1
                else do
                  let retF = outletForced (axLowL2, axUppL2) (getPosoutI rules i) (posoutX !! i)
                  if s !! i == -1 && retF /= 0 then do
                    putStr $ show depth ++ " Positioned outlet "
                    putStrLn $ show ((rules ^. _1) !! pos) ++ "," ++ show x
                            ++ " can't be forced, because it forces "
                            ++ show ((rules ^. _1) !! i)   ++ "," ++ show (posoutX !! i)
                    return 0
                  else
                    loop5_2 (i + 1)
          _good2 <- liftIO $ loop5_2 0
          let good = 0
          if good /= 0 then do
            -- recursion with PO forced
            liftIO . putStrLn $ show depth ++ " Starting recursion with "
            liftIO . putStrLn $ show ((rules ^. _1) !! pos) ++ ", " ++ show x ++ " forced"
            lift . runMaybeT $ checkBound (axLowL2, axUppL2) (s & ix pos .~ 1) maxch (pos + 1) (depth + 1)
            liftIO $ putStr ""
          else
            liftIO $ putStr ""

          -- 5.3. rejecting positioned outlet PO
          liftIO . putStrLn $ show depth ++ " Rejecting positioned outlet "
          liftIO . putStrLn $ show ((rules ^. _1) !! pos) ++ ", " ++ show x
          let s2         = s  & ix pos .~ (-1)
              allowedch2 = allowedch - ((rules ^. _3) !! pos)
          if allowedch2 + forcedch <= maxch then do
            liftIO $ putStrLn "Inequality holds."
            empty -- 正常終了
          else
            liftIO $ putStrLn ""
          loop5 (pos + 1) allowedch2 s2
  loop5 0 allowedch s

  -- 6.
  return "checkBound end." -- error "Unexpected error 101"



