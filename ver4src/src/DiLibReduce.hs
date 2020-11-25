module DiLibReduce where

import DiLibCConst
    ( TpReducePack,
      TpPosout,
      TpEdgelist,
      TpGoodConf,
      TpVertices,
      TpAdjmat,
      TpAxleI,
      cartvert,
      maxelist,
      maxastack )
import Control.Applicative       (empty)
import Control.Arrow             ((<<<))
import Control.Lens
    ( (&), (^?!), (^.), (.~), Ixed(ix), Field1(_1), Field2(_2) )
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS   (RWST(..), ask, get, put)
import Data.Maybe                (isNothing)


reduce :: MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
reduce = do
  (gConfs, _, deg)                                              <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, _, edgelist), posoutX) <- lift get

  -- 1.
  let noconf = 633
      loop1 naxles --型がMaybeT
        | naxles <= 0 || naxles >= maxastack      = return "1. end."
        | otherwise = do

          -- 1.1.
          (liftIO . putStrLn) "Axle from stack:"
          let naxles2   = naxles - 1
              adjmat2   = getAdjmat   (aSLow !! aSLev, aSUpp !! aSLev)          deg
              edgelist2 = getEdgelist (aSLow !! aSLev, aSUpp !! aSLev) edgelist deg
          (lift . put) (((aSLow, aSUpp, aSLev), used, image, adjmat2, edgelist2), posoutX)
          -- ##### printf debug #####
          -- (liftIO . print) adjmat2
          -- (liftIO . print) $ (edgelist2 !! 5) !! 5
          -- (liftIO . print) $ aSUpp !! aSLev
          -- (liftIO . print) image -- 最初はall 0
          -- ##### printf debug #####

          -- 1.2.
          let loop1_2 h = --型がMaybeT
                if h >= noconf then do
                  -- ##### printf debug #####
                  -- ((_, _, imageZ, _, _), _) <- lift get
                  -- (liftIO . print) imageZ
                  -- ##### printf debug #####
                  --   どのみち、ループ終了後失敗終了だから、ここで落とす
                  (liftIO . putStrLn) "Not reducible."
                  empty -- 失敗終了
                else do
                  retSC <- lift . runMaybeT $ subConf (aSLow !! aSLev, aSUpp !! aSLev) (gConfs !! h)
                  if isNothing retSC then
                    (return . show) h -- 正常終了
                  else
                    loop1_2 (h + 1) -- 再帰
          ret1_2      <- loop1_2 0
          let h        = read ret1_2 :: Int
              redverts = ((gConfs !! h) ^. _1) !! 1
              redring  = ((gConfs !! h) ^. _2) !! 1

          -- 1.3. omitted
          --if (conf != NULL)
          --  CheckIso(conf[h], B, image, lineno);
          -- Double-check isomorphism

          -- 1.4.
          let loop1_4 i n up = -- IO
                let v      = image !! i
                    aSLowV = aSLow !! aSLev !! v
                    aSUppV =    up !! aSLev !! v
                in if i > redverts then
                  return (n, up)
                else
                  if aSLowV == aSUppV then
                    loop1_4 (i + 1) n up
                  else do
                    putStr     "Lowering upper bound of vertex "
                    putStrLn $ show v
                                ++ " to "
                                ++ show aSUppV
                                ++ " and adding to stack"
                    if n >= maxastack then
                      error "More than %d elements in axle stack needed"
                    else
                      loop1_4 (i + 1) (n + 1) (aSUpp & (ix aSLev <<< ix v) .~ (aSUppV - 1))
          (naxles3, aSUpp2) <- liftIO $ loop1_4 (redring + 1) naxles2 aSUpp
          (lift . put) (((aSLow, aSUpp2, aSLev), used, image, adjmat2, edgelist2), posoutX)

          -- 1.5. 再帰
          loop1 naxles3
  loop1 1

  -- 2.
  return "reduce end." -- 正常終了


getAdjmat :: TpAxleI -> Int -> TpAdjmat
getAdjmat (_, axUppL) deg =
  let
    adjmat         = replicate cartvert $ replicate cartvert (-1)
    loop1 i adjmat =
      if i > deg then adjmat
      else
        let adjmat2 = getAdjmatSub deg axUppL adjmat i
        in loop1 (i + 1) adjmat2
  in loop1 1 adjmat

data Way = Forward | Backward deriving Eq
chgAdjmat :: TpAdjmat -> Int -> Int -> Int -> Way -> TpAdjmat
chgAdjmat adjmat a b c way =
  if way == Forward then
    let adjmat2 = adjmat  & (ix a <<< ix b) .~ c
        adjmat3 = adjmat2 & (ix c <<< ix a) .~ b
        adjmat4 = adjmat3 & (ix b <<< ix c) .~ a
    in adjmat4
  else
    let adjmat2 = adjmat  & (ix a <<< ix b) .~ c
        adjmat3 = adjmat2 & (ix b <<< ix c) .~ a
        adjmat4 = adjmat3 & (ix c <<< ix a) .~ b
    in adjmat4

getAdjmatSub :: Int -> [Int] -> TpAdjmat -> Int -> TpAdjmat
getAdjmatSub deg bUpp adjmat i =
  let
    h       = if i == 1 then deg else i - 1
    a       = deg + h
    adjmat2 = chgAdjmat adjmat  0 h i Forward
    adjmat3 = chgAdjmat adjmat2 i h a Forward
  in if bUpp !! i < 9 then
    doFan deg i (bUpp !! i) adjmat3
  else
    adjmat3

doFan :: Int -> Int -> Int -> TpAdjmat -> TpAdjmat
doFan deg i k adjmat =
  let
    a       = if i == 1 then 2 * deg else deg + i - 1
    b       =     deg + i
    c       = 2 * deg + i
    adjmat2 = chgAdjmat adjmat  i a c Backward
    d       = 3 * deg + i
    adjmat3 = chgAdjmat adjmat2 i c d Backward
    e       = 4 * deg + i
    adjmat4 = chgAdjmat adjmat3 i d e Backward
    ret
      | k == 5    = chgAdjmat adjmat  i a b Backward
      | k == 6    = chgAdjmat adjmat2 i c b Backward
      | k == 7    = chgAdjmat adjmat3 i d b Backward
      | otherwise = chgAdjmat adjmat4 i e b Backward
  in ret


getEdgelist :: TpAxleI -> TpEdgelist -> Int -> TpEdgelist
getEdgelist (axLowL, axUppL) _edgelist deg =
  let
    edgelist23       = replicate 12 $ replicate 9 $ replicate maxelist 0 --edgelist22 & (ix 11 <<< ix 8 <<< ix 0) .~ 0
    loop1 i edgelist =
      if i > deg then edgelist
      else
        let edgelist2 = getEdgelistSub axLowL axUppL edgelist deg i
        in loop1 (i + 1) edgelist2
  in loop1 1 edgelist23

getEdgelistSub :: TpVertices -> TpVertices -> TpEdgelist -> Int -> Int -> TpEdgelist
getEdgelistSub bLow bUpp edgelist deg i =
  let
    edgelist2  = addToList edgelist   0 i bUpp
    h          = if i == 1 then deg else i - 1
    edgelist3  = addToList edgelist2  i h bUpp
    a          = deg + h
    b          = deg + i
    edgelist4  = addToList edgelist3  i a bUpp
    edgelist5  = addToList edgelist4  i b bUpp
    bLowI      = bLow !! i
    bUppI      = bUpp !! i
    c          = 2 * deg + i
    edgelist6  = addToList edgelist5  a c bUpp
    edgelist7  = addToList edgelist6  i c bUpp
    d          = 3 * deg + i;
    edgelist8  = addToList edgelist7  c d bUpp
    edgelist9  = addToList edgelist8  i d bUpp
    e          = 4 * deg + i
    edgelist10 = addToList edgelist9  d e bUpp
    edgelist11 = addToList edgelist10 i e bUpp
    ret
      | bLowI /= bUppI = edgelist5
      | bUppI == 5     = addToList edgelist5  a b bUpp
      | bUppI == 6     = addToList edgelist7  b c bUpp
      | bUppI == 7     = addToList edgelist9  b d bUpp
      | bUppI == 8     = addToList edgelist11 b e bUpp
      | otherwise      = error "Unexpected error in `GetEdgeList'"
  in ret

--[5][5]	0x002c0ec0 {12, 2, 1, 1, 2, 3, 2, 2, 3, 4, 3, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...}
-- adds the pair u,v to edgelist
-- この関数の実装はひどい
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> TpEdgelist
addToList edgelist u v degree =
  let
    a           = degree !! u
    b           = degree !! v
    eHead1      = (((edgelist ^?! ix a) ^?! ix b) ^?! ix 0)
    edgelist1_2 = edgelist    & (ix a <<< ix b <<< ix (eHead1 + 1)) .~ u
    edgelist1_3 = edgelist1_2 & (ix a <<< ix b <<< ix (eHead1 + 2)) .~ v
    edgelist1_4 = edgelist1_3 & (ix a <<< ix b <<< ix 0)            .~ (eHead1 + 2)
    bool1       = a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
    bool1_1     = eHead1 + 2 >= maxelist
    eHead2      = (((edgelist ^?! ix b) ^?! ix a) ^?! ix 0)
    edgelist2_2 = edgelist    & (ix b <<< ix a <<< ix (eHead2 + 1)) .~ v
    edgelist2_3 = edgelist2_2 & (ix b <<< ix a <<< ix (eHead2 + 2)) .~ u 
    edgelist2_4 = edgelist2_3 & (ix b <<< ix a <<< ix 0)            .~ (eHead2 + 2)
    edgelist2_2_2 = if a == b then edgelist1_4   & (ix b <<< ix a <<< ix (eHead2 + 3)) .~ v
                              else edgelist1_4   & (ix b <<< ix a <<< ix (eHead2 + 1)) .~ v
    edgelist2_3_2 = if a == b then edgelist2_2_2 & (ix b <<< ix a <<< ix (eHead2 + 4)) .~ u
                              else edgelist2_2_2 & (ix b <<< ix a <<< ix (eHead2 + 2)) .~ u
    edgelist2_4_2 = if a == b then edgelist2_3_2 & (ix b <<< ix a <<< ix 0)            .~ (eHead2 + 4)
                              else edgelist2_3_2 & (ix b <<< ix a <<< ix 0)            .~ (eHead2 + 2)
    bool2       = b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)
    bool2_1     = eHead2 + 2 >= maxelist
    ret
      | bool1 &&     bool1_1 = error "More than %d entries in edgelist needed 1"
      | bool1 && not bool1_1 && not (bool2 && not bool2_1) = edgelist1_4
      | bool1 && not bool1_1 &&     (bool2 && not bool2_1) = edgelist2_4_2
      | bool2 &&     bool2_1 = error "More than %d entries in edgelist needed 2"
      | bool2 && not bool2_1 = edgelist2_4
      | otherwise            = edgelist
  in ret


subConf :: TpAxleI -> TpGoodConf -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
subConf (_, axUppL) gC@(_, _, _, qXi) = do

  ((_, _, _, _, edgelist), _) <- lift get

  -- 1.
  let qXi0      = head qXi
      qXi1      = qXi !! 1
      pedgeHead = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0
      loop1 i   = -- MaybeT
        if i > pedgeHead then
          return "subConf end." -- 失敗終了
        else do
          let x = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix i
              y = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1)
          ret1 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 1
          ret2 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 0
          if isNothing ret1 && isNothing ret2 then loop1 (i + 2) -- 再帰
          else empty            -- 正常終了
  loop1 1

rootedSubConf :: TpVertices -> TpGoodConf -> Int -> Int -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
rootedSubConf degree gConf@(_, _, qZ, _) x y clockwise = do

  (_, _, deg)                                                <- lift ask
  (((aSLow, aSUpp, aSLev), _, _, adjmat, edgelist), posoutX) <- lift get

  -- 1.
  let used2  = replicate cartvert False
      image2 = replicate cartvert (-1) -- !!
      image3 = image2 & ix 0 .~ clockwise
      qZ0    = head qZ
      qZ1    = qZ !! 1
      image4 = image3 & ix qZ0 .~ x
      image5 = image4 & ix qZ1 .~ y
      used3  = used2 & ix x .~ True
      used4  = used3 & ix y .~ True

  -- 2.
  (retB, used6, image6, _) <- rootedSubConfSub2 2 used4 image5 gConf adjmat clockwise degree
  (lift . put) (((aSLow, aSUpp, aSLev), used6, image6, adjmat, edgelist), posoutX)
  if retB then
    (liftIO . putStr) ""
  else
    empty -- 失敗終了

  -- 3. test if image is well-positioned
  rootedSubConfSub3 1 used6 deg -- empty 失敗終了の可能性もある

  -- 4.
  return "rootedSubConf end." -- 正常終了


rootedSubConfSub2 :: Int -> [Bool] -> [Int] -> TpGoodConf -> TpAdjmat -> Int -> TpVertices
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) (Bool, [Bool], [Int], Int)
rootedSubConfSub2 j used image (qU, qV, qZ, qXi) adjmat clockwise degree =
  let qUQ = qU !! j
  in if qUQ < 0 then
    return (True, used, image, qUQ)
  else do
    let
      qVQ      = qV     !! j
      qZQ      = qZ     !! j
      qXiQ     = qXi    !! j
      imageQUQ = image  !! qUQ
      imageQVQ = image  !! qVQ
      w        = if clockwise == 0 then  (adjmat ^?! ix imageQVQ) ^?! ix imageQUQ
                  else                   (adjmat ^?! ix imageQUQ) ^?! ix imageQVQ
      degreeW  = degree !! w
      usedW    = used   !! w
    if (w == -1) || (qXiQ /= 0 && qXiQ /= degreeW) || usedW then
      return (False, used, image, qUQ)
    else
      rootedSubConfSub2 (j + 1) (used & ix w .~ True) (image & ix qZQ .~ w) (qU, qV, qZ, qXi) adjmat clockwise degree


rootedSubConfSub3 :: Int -> [Bool] -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
rootedSubConfSub3 j used6 deg =
  let
    degJ = if j == 1 then 2 * deg else deg + j - 1
    ret
      | j > deg                                                 = return "loop3 end."
      | not (used6 !! j) && used6 !! (deg + j) && used6 !! degJ = empty -- 失敗終了
      | otherwise                                               = rootedSubConfSub3 (j + 1) used6 deg
  in ret



