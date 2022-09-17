module DiLibReduce( reduce ) where

import CoLibCConst
    ( cartvert,
      maxastack,
      maxelist,
      TpAdjmat,
      TpAxle,
      TpAxleI,
      TpEdgelist,
      TpGoodConf,
      TpPosout,
      TpReducePack,
      TpVertices )
import Control.Applicative       (empty)
import Control.Arrow             ((<<<))
import Control.Lens
    ( (&), (^?!), (^.), (.~), Ixed(ix), Field1(_1), Field2(_2) )
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS   (RWST(..), ask, get, put)
import Data.Maybe                (isNothing)
import Text.Printf               ( printf )


{-
reduce()
  -- 0.
  -- 1.
    getAdjmat()
    getEdgelist()
  -- 2.
    reduceSub2()
  -- 3. omitted
  -- 4.
    reduceSub4()|
  -- 5. recursion

  reduceSub2()
    subConf()
      rootedSubConf()
        -- 1.
        -- 2.
          rootedSubConfSub2()|
        -- 3.
          rootedSubConfSub3()|
        -- 4.

~~~ pure ~~~
  getAdjmat()
    getAdjmatSub()
      chgAdjmat()|
      doFan()
        chgAdjmat()|
  getEdgelist()
    getEdgelistSub()
      addToList()|
-}

reduce :: Int -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
reduce naxles
  | naxles <= 0 || naxles >= maxastack = return "end reduce" -- true end
  | otherwise                          = do

      -- 0.
      (gConfs, _, deg)                                                 <- lift ask
      ((aS@(aSLow, aSUpp, aSLev), used, image, _, _edgelist), posoutX) <- lift get

      -- 1.
      (liftIO . putStrLn) "Axle from stack:"
      let noconf     = 633
          naxles2    = naxles - 1
          adjmat2    = getAdjmat   (aSLow !! aSLev, aSUpp !! aSLev)          deg
          edgelist23 = replicate 12 $ replicate 9 $ replicate maxelist 0 --edgelist22 & (ix 11 <<< ix 8 <<< ix 0) .~ 0
          edgelist2  = getEdgelist (aSLow !! aSLev, aSUpp !! aSLev) edgelist23 deg 1
      (lift . put) (((aSLow, aSUpp, aSLev), used, image, adjmat2, edgelist2), posoutX)

      -- 2.
      ret1_2      <- reduceSub2 0 noconf -- empty 失敗終了の可能性もある
      liftIO $ printf "##########################\n"
      liftIO $ printf "naxles: %d   aSLev: %d\n" naxles aSLev
      liftIO $ printf "gConfNo: %s\n" ret1_2
      liftIO $ printf "##########################\n"
      let h        = read ret1_2 :: Int
          redverts = ((gConfs !! h) ^. _1) !! 1
          redring  = ((gConfs !! h) ^. _2) !! 1

      -- 3. omitted
      --if (conf != NULL)
      --  CheckIso(conf[h], B, image, lineno);
      -- Double-check isomorphism

      -- 4.
      (naxles3, aSUpp2) <- liftIO $ reduceSub4 (redring + 1) naxles2 aS image redverts
      (lift . put) (((aSLow, aSUpp2, aSLev), used, image, adjmat2, edgelist2), posoutX)

      -- 5. recursion
      reduce naxles3


reduceSub2 :: Int -> Int -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
reduceSub2 h noconf = do
  (gConfs, _, _)                           <- lift ask
  (((aSLow, aSUpp, aSLev), _, _, _, _), _) <- lift get
  retSC <- lift . runMaybeT $ subConf (aSLow !! aSLev, aSUpp !! aSLev) (gConfs !! h) 1
  let ret
        | h >= noconf     = do
            --   どのみち、ループ終了後失敗終了だから、ここで落とす
            (liftIO . printf) "Not reducible.\n"
            empty                                     -- fail end
        | isNothing retSC = (return . show) h         -- true end
        | otherwise       = reduceSub2 (h + 1) noconf -- recursion
  ret

reduceSub4 :: Int -> Int -> TpAxle -> [Int] -> Int -> IO (Int, [[Int]])
reduceSub4 i n aS@(aSLow, up, aSLev) image redverts = ret
  where
    v      = image !! i
    aSLowV = aSLow !! aSLev !! v
    aSUppV =    up !! aSLev !! v
    ret
      | i > redverts     = return (n, up)
      | aSLowV == aSUppV = reduceSub4 (i + 1) n aS image redverts
      | n >= maxastack   = error "More than %d elements in axle stack needed"
      | otherwise = do
          printf "Lowering upper bound of vertex %d to %d and adding to stack\n" v aSUppV
          reduceSub4 (i + 1) (n + 1) (aSLow, up & (ix aSLev <<< ix v) .~ (aSUppV - 1), aSLev) image redverts


subConf :: TpAxleI -> TpGoodConf -> Int -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
subConf axL@(_, axUppL) gC@(_, _, _, qXi) i = do
  ((_, _, _, _, edgelist), _) <- lift get
  let
    qXi0      = head qXi
    qXi1      = qXi !! 1
    pedgeHead = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0
    x         = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix i
    y         = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1)
  ret1 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 1
  ret2 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 0
  let
    ret
      | i > pedgeHead                    = return "end subConf"   -- fail end
      | isNothing ret1 && isNothing ret2 = subConf axL gC (i + 2) -- recursion
      | otherwise                        = empty                  -- true end
  ret


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
    liftIO $ printf ""
  else
    empty -- fail end

  -- 3. test if image is well-positioned
  _ <- rootedSubConfSub3 1 used6 deg -- empty 失敗終了の可能性もある

  -- 4.
  return "end rootedSubConf" -- true end


rootedSubConfSub2 :: Int -> [Bool] -> [Int] -> TpGoodConf -> TpAdjmat -> Int -> TpVertices
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) (Bool, [Bool], [Int], Int)
rootedSubConfSub2 j used image (qU, qV, qZ, qXi) adjmat clockwise degree = ret
  where
    qUQ = qU !! j
    qVQ      = qV     !! j
    qZQ      = qZ     !! j
    qXiQ     = qXi    !! j
    imageQUQ = image  !! qUQ
    imageQVQ = image  !! qVQ
    w        =  if clockwise == 0 then (adjmat ^?! ix imageQVQ) ^?! ix imageQUQ
                else                   (adjmat ^?! ix imageQUQ) ^?! ix imageQVQ
    degreeW  = degree !! w
    usedW    = used   !! w
    ret
      | qUQ < 0                                              = return (True,  used, image, qUQ)
      | (w == -1) || (qXiQ /= 0 && qXiQ /= degreeW) || usedW = return (False, used, image, qUQ)
      | otherwise                                            =
          rootedSubConfSub2 (j + 1) (used & ix w .~ True) (image & ix qZQ .~ w) (qU, qV, qZ, qXi) adjmat clockwise degree


rootedSubConfSub3 :: Int -> [Bool] -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
rootedSubConfSub3 j used6 deg = ret
  where
    degJ = if j == 1 then 2 * deg else deg + j - 1
    ret
      | j > deg                                                 = return "end loop3"
      | not (used6 !! j) && used6 !! (deg + j) && used6 !! degJ = empty -- fail end
      | otherwise                                               = rootedSubConfSub3 (j + 1) used6 deg


-- ~~~ pure function ~~~
getAdjmat :: TpAxleI -> Int -> TpAdjmat
getAdjmat (_, axUppL) deg = loop1 1 adjmat
  where
    adjmat        = replicate cartvert $ replicate cartvert (-1)
    loop1 i adjmat0
      | i > deg   = adjmat0
      | otherwise = loop1 (i + 1) adjmat2 
          where adjmat2 = getAdjmatSub deg axUppL adjmat0 i


getAdjmatSub :: Int -> [Int] -> TpAdjmat -> Int -> TpAdjmat
getAdjmatSub deg bUpp adjmat i
  | bUpp !! i < 9 = doFan deg i (bUpp !! i) adjmat3
  | otherwise     = adjmat3
  where
    h       = if i == 1 then deg else i - 1
    a       = deg + h
    adjmat2 = chgAdjmat adjmat  0 h i Forward
    adjmat3 = chgAdjmat adjmat2 i h a Forward


doFan :: Int -> Int -> Int -> TpAdjmat -> TpAdjmat
doFan deg i k adjmat = ret
  where
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


data Way = Forward | Backward deriving Eq
chgAdjmat :: TpAdjmat -> Int -> Int -> Int -> Way -> TpAdjmat
chgAdjmat adjmat a b c Forward  = adjmat4
  where
    adjmat2 = adjmat  & (ix a <<< ix b) .~ c
    adjmat3 = adjmat2 & (ix c <<< ix a) .~ b
    adjmat4 = adjmat3 & (ix b <<< ix c) .~ a
chgAdjmat adjmat a b c Backward = adjmat4
  where
    adjmat2 = adjmat  & (ix a <<< ix b) .~ c
    adjmat3 = adjmat2 & (ix b <<< ix c) .~ a
    adjmat4 = adjmat3 & (ix c <<< ix a) .~ b


getEdgelist :: TpAxleI -> TpEdgelist -> Int -> Int -> TpEdgelist
getEdgelist (axLowL, axUppL) edgelist deg i
  | i > deg   = edgelist
  | otherwise = getEdgelist (axLowL, axUppL) edgelist2 deg (i + 1)
      where edgelist2 = getEdgelistSub axLowL axUppL edgelist deg i


getEdgelistSub :: TpVertices -> TpVertices -> TpEdgelist -> Int -> Int -> TpEdgelist
getEdgelistSub bLow bUpp edgelist deg i = ret
  where
    edgelist2  = (addToList i 0 bUpp . addToList 0 i bUpp) edgelist
    h          = if i == 1 then deg else i - 1
    edgelist3  = (addToList h i bUpp . addToList i h bUpp) edgelist2
    a          = deg + h
    b          = deg + i
    edgelist4  = (addToList a i bUpp . addToList i a bUpp) edgelist3
    edgelist5  = (addToList b i bUpp . addToList i b bUpp) edgelist4
    bLowI      = bLow !! i
    bUppI      = bUpp !! i
    c          = 2 * deg + i
    edgelist6  = (addToList c a bUpp . addToList a c bUpp) edgelist5
    edgelist7  = (addToList c i bUpp . addToList i c bUpp) edgelist6
    d          = 3 * deg + i
    edgelist8  = (addToList d c bUpp . addToList c d bUpp) edgelist7
    edgelist9  = (addToList d i bUpp . addToList i d bUpp) edgelist8
    e          = 4 * deg + i
    edgelist10 = (addToList e d bUpp . addToList d e bUpp) edgelist9
    edgelist11 = (addToList e i bUpp . addToList i e bUpp) edgelist10
    ret
      | bLowI /= bUppI = edgelist5
      | bUppI == 5     = (addToList b a bUpp . addToList a b bUpp) edgelist5
      | bUppI == 6     = (addToList c b bUpp . addToList b c bUpp) edgelist7
      | bUppI == 7     = (addToList d b bUpp . addToList b d bUpp) edgelist9
      | bUppI == 8     = (addToList e b bUpp . addToList b e bUpp) edgelist11
      | otherwise      = error "Unexpected error in `GetEdgeList'"


addToList :: Int -> Int -> TpVertices -> TpEdgelist -> TpEdgelist
addToList u v degree edgelist = ret
  where
    a           = degree !! u
    b           = degree !! v
    eHead1      = ((edgelist ^?! ix a) ^?! ix b) ^?! ix 0
    edgelist1_2 = edgelist    & (ix a <<< ix b <<< ix (eHead1 + 1)) .~ u
    edgelist1_3 = edgelist1_2 & (ix a <<< ix b <<< ix (eHead1 + 2)) .~ v
    edgelist1_4 = edgelist1_3 & (ix a <<< ix b <<< ix 0)            .~ (eHead1 + 2)
    bool1       = a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
    bool1_1     = eHead1 + 2 >= maxelist
    ret
      | bool1 &&     bool1_1 = error "More than %d entries in edgelist needed"
      | bool1 && not bool1_1 = edgelist1_4
      | otherwise            = edgelist



