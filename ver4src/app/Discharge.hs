{-
◆author: righ1113
◆動かし方
1. $ stack run discharge-exe
2. > 「07」を入力してEnter。
3. 中心の次数07のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK（今は表示されない）
1. $ stack ghci --main-is ver4src:exe:discharge-exe
2. > main
 . > :l app/Discharge
-}
module Main where
-- module Discharge where


{-
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import Lib (myLoop)
-}
import Control.Applicative       (empty)
import Control.Arrow             ((<<<))
import Control.Lens              ((&), (.~), ix, (^?!), _1, _2, _3, _4, _5, _6, (^.))
--import Data.Maybe                (fromJust)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.RWS   (RWST(..), ask, get, put)


verts      = 27             -- max number of vertices in a free completion + 1
confs      = 640            -- max number of configurations
maxval     = 12
cartvert   = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle
infty      = 12             -- the "12" in the definition of limited part
maxoutlets = 110            -- max number of outlets
maxelist   = 134            -- length of edgelist[a][b]
maxastack  = 5              -- max height of Astack (see "Reduce")
maxlev     = 12             -- max level of an input line + 1
difNouts   = [0, 0, 0, 0, 0, 0, 0, 103, 103, 103, 103, 103]


type TpAxle          = ([[Int]], [[Int]], Int)
type TpAxleI         = ([Int], [Int])
type TpCond          = ([Int], [Int])
type TpAdjmat        = [[Int]]
type TpVertices      = [Int]
type TpGoodConf      = ([Int], [Int], [Int], [Int])
type TpEdgelist      = [[[Int]]]
type TpPosout        = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]])
type TpPosoutI       = (Int, Int, Int, [Int], [Int], [Int])
-- type TpReducePack    = ([Int], [Int], TpAdjmat, TpEdgelist, [TpGoodConf])
type TpReducePack    = (TpAxle, [Bool], TpVertices, TpAdjmat, TpEdgelist)
type TpConfPack      = (Bool, Int, [Bool], TpVertices, Int)


main :: IO ()
main = do

  putStrLn "これは四色定理の放電法をおこなうプログラムです。"
  putStrLn "中心の次数07, 08, 09, 10, 11のいずれかを入力してください。"

  -- deg
  degStr <- getLine
  let deg = read degStr :: Int

  -- TpAxle
  let axles0 = replicate maxlev $ replicate cartvert 0
  let axLow  = take cartvert ([deg] ++ replicate (5 * deg) 5     ++ repeat 0) : axles0
  let axUpp  = take cartvert ([deg] ++ replicate (5 * deg) infty ++ repeat 0) : axles0

  -- TpCond
  let nn = replicate maxlev 0
  let mm = replicate maxlev 0

  -- TpPosout
  rulesStr   <- readFile $ "4ctdata/DiRules" ++ degStr ++ ".txt"
  let rules   = read rulesStr :: TpPosout -- read rules, compute outlets
  let posoutX = replicate (2 * maxoutlets) 0

  -- sym
  let symNum = replicate (2 * maxoutlets) 0
  let symNol = replicate (2 * maxoutlets) 0
  let symVal = replicate (2 * maxoutlets) 0
  let symPos = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let symLow = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let symUpp = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  -- TpReducePack
  let aSLow    = replicate (maxlev + 1) $ replicate cartvert 0
  let aSUpp    = replicate (maxlev + 1) $ replicate cartvert 0
  let bLow     = replicate cartvert 0
  let bUpp     = replicate cartvert 0
  let adjmat   = replicate cartvert $ replicate cartvert 0
  let edgelist = replicate 12 $ replicate 9 $ replicate maxelist 0
  let used     = replicate cartvert False
  let image    = replicate cartvert 0
  gConfsStr   <- readFile "4ctdata/DiGoodConfs.txt"
  let gConfs   = read gConfsStr :: [TpGoodConf] -- read GoodConfs set

  -- Tactics
  inStr <- readFile $ "4ctdata/DiTactics" ++ degStr ++ ".txt"

  -- mainLoop
  (ret, s, w)
    <- runRWST (mainLoop (nn, mm)
                         (symNum, symNol, symVal, symPos, symLow, symUpp)
                         0
                         (axLow, axUpp, 0)
                         (tail (map words (lines inStr))) )
               (gConfs, rules, deg)                                          -- read
               (((aSLow, aSUpp, 0), used, image, adjmat, edgelist), posoutX) -- state

  -- final check
  if ret == "Q.E.D." then
    putStrLn $ "中心の次数" ++ degStr -- ++ "のグラフは、電荷が負になるか、近くに好配置があらわれるかです。"
  else
    putStrLn "失敗です。"

  putStrLn "プログラムは正常終了しました。"


mainLoop :: TpCond -> TpPosout -> Int -> TpAxle -> [[String]]
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
mainLoop (nn, mm) sym nosym ax@(axLow, axUpp, axLev) tactics
  | axLev >= maxlev = error "More than %d levels"
  | axLev < 0       = return $ head $ head tactics
  | otherwise     = let nowTac = head tactics in
      case nowTac !! 1 of
        "S" -> do
                liftIO $ putStrLn $ "Symmetry  " ++ show nowTac
                --checkSymmetry (tail (tail (head tactics))) axles posout nosym
                --mainLoop rP posout (nn, mm) deg nosym (low, upp, lev - 1) (tail tactics)
                return "Q.E.D."
        "R" -> do
                liftIO $ putStrLn $ "Reduce  " ++ show nowTac
                (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- get
                put ( ( (aSLow & ix 0 .~ axLow !! axLev, aSUpp & ix 0 .~ axUpp !! axLev, aSLev),
                        used, image, adjmat, edgelist ),
                      posoutX )
                ret <- runMaybeT reduce
                if ret == Nothing then
                  error "Reducibility failed"
                else
                  mainLoop (nn, mm) sym nosym (axLow, axUpp, axLev - 1) (tail tactics)
        "H" -> do
                liftIO $ putStrLn $ "Hubcap  " ++ show nowTac
                checkHubcap (tail (tail (head tactics))) ax
                --mainLoop rP posout' (nn, mm) deg nosym (low, upp, lev - 1) (tail tactics)
                return "Q.E.D."
        "C" -> do
                liftIO $ putStrLn $ "Condition  " ++ show nowTac
                {-let n = read (head tactics !! 2) :: Int
                let m = read (head tactics !! 3) :: Int
                nosym2                   <- checkCondition1 (nn, mm) deg axles n m nosym
                (cond2, (low2, upp2, _)) <- checkCondition2 (nn, mm) axles n m-}
                mainLoop (nn, mm) sym nosym (axLow, axUpp, axLev + 1) (tail tactics)
        _   -> error "Invalid instruction"



-- ##################################################################################################################
--   Symmetry
-- ##################################################################################################################
getPosoutI :: TpPosout -> Int -> TpPosoutI
getPosoutI (num, nol, val, pos, low, upp) i
  = (num !! i, nol !! i, val !! i, pos !! i, low !! i, upp !! i)

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int
outletForced axL@(axLowL, axUppL) (numI, nolI, valI, posI, lowI, uppI) pXI = 66

outletPermitted :: TpAxleI -> TpPosoutI -> Int -> Int
outletPermitted axL@(axLowL, axUppL) (numI, nolI, valI, posI, lowI, uppI) pXI = 67

{-
checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, upp, lev) posout@(number, nolines, value, pos, plow, pupp, xx) nosym =
  let [k, epsilon, level, line] = map read str :: [Int]
      i                         = fromJust $ find (==line) number
      pI                        = (number !! i, nolines !! i, value !! i, pos !! i, plow !! i, pupp !! i, xx !! i)
    -- ★mark
  in if k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1 then                       error "Illegal symmetry"
     else if i >= nosym then                                                                    error "No symmetry as requested"
          else if nolines !! i /= level + 1 then                                                error "Level mismatch"
               else if epsilon == 0 && outletForced (low !! lev, upp !! lev) pI (k+1) /= 1 then error "Invalid symmetry"
                    else if reflForced aA /= 1 then                                             error "Invalid reflected symmetry"
                                               else                                             putStrLn "  checkSymmetry OK."

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int
outletForced aAI@(lowI, uppI) (numberI, nolinesI, valueI, posI, plowI, puppI, xxI) y =
  let deg = head lowI
      y2  = y - 1
      f n x cont
        | n == 0    = n
        | otherwise = cont $ outletForcedSub aAI deg posI y2 n x
      ret           = myLoop f id 1 [0, 1 .. nolinesI]
  in if ret == 0
    then 0
    else valueI
outletForcedSub :: TpAxleI -> Int -> [Int] -> Int -> Int -> Int -> Int
outletForcedSub aAI@(lowI, uppI) deg posI y _ i =
  let p  = posI !! i
      p2 = if (y + (p - 1) `mod` deg) < deg then p + y else p + y - deg
  in if (lowI !! i > lowI !! p2) || (uppI !! i < uppI !! p2)
    then 0
    else 1

reflForced :: TpAxle -> Int
reflForced _ = 1
-}



-- ##################################################################################################################
--   Reduce
-- ##################################################################################################################
reduce :: MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
reduce = do
  (gConfs, rules, deg)                                              <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- lift get

  -- 1.
  let noconf = 633
      loop1 naxles --型がMaybeT
        | naxles <= 0 || naxles >= maxastack      = return "1. end."
        | otherwise = do

          -- 1.1.
          (liftIO . putStr) "Axle from stack:"
          let naxles2   = naxles - 1
              adjmat2   = getAdjmat   (aSLow !! aSLev, aSUpp !! aSLev)          deg
              edgelist2 = getEdgelist (aSLow !! aSLev, aSUpp !! aSLev) edgelist deg
          (lift . put) (((aSLow, aSUpp, aSLev), used, image, adjmat2, edgelist2), posoutX)

          -- 1.2.
          let loop1_2 h k = --型がMaybeT
                if h >= noconf then do
                  -- どのみち、ループ終了後異常終了だから、ここで落とす
                  (liftIO . putStrLn) "Not reducible."
                  (return . show) h --empty
                else do
                  retSC <- lift . runMaybeT $ subConf (aSLow !! aSLev, aSUpp !! aSLev) (gConfs !! h)
                  if retSC == Nothing then
                    (return . show) h -- 正常終了
                  else
                    loop1_2 (h + 1) k -- 再帰
          ret1_2      <- loop1_2 0 7
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
getAdjmat axL@(_, axUppL) deg =
  let adjmat         = replicate cartvert $ replicate cartvert (-1)
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
  let h       = if i == 1 then deg else i - 1
      a       = deg + h
      adjmat2 = chgAdjmat adjmat 0 h i Forward
      adjmat3 = chgAdjmat adjmat2 i h a Forward
  in if bUpp !! i < 9 then
    doFan deg i (bUpp !! i) adjmat3
  else
    adjmat3

doFan :: Int -> Int -> Int -> TpAdjmat -> TpAdjmat
doFan deg i k adjmat =
  let a       = if i == 1 then 2 * deg else deg + i - 1
      b       = deg + i
      c       = 2 * deg + i
      adjmat2 = chgAdjmat adjmat i a c Backward
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
getEdgelist axL@(axLowL, axUppL) edgelist deg =
  let
    edgelist2        =  edgelist  & (ix 5  <<< ix 5 <<< ix 0) .~ 0
    edgelist3        =  edgelist2 & (ix 6  <<< ix 5 <<< ix 0) .~ 0
    edgelist4        =  edgelist3 & (ix 6  <<< ix 6 <<< ix 0) .~ 0
    edgelist5        =  edgelist4 & (ix 7  <<< ix 5 <<< ix 0) .~ 0
    edgelist6        =  edgelist5 & (ix 7  <<< ix 6 <<< ix 0) .~ 0
    edgelist7        =  edgelist6 & (ix 7  <<< ix 7 <<< ix 0) .~ 0
    edgelist8        =  edgelist7 & (ix 8  <<< ix 5 <<< ix 0) .~ 0
    edgelist9        =  edgelist8 & (ix 8  <<< ix 6 <<< ix 0) .~ 0
    edgelist10       =  edgelist9 & (ix 8  <<< ix 7 <<< ix 0) .~ 0
    edgelist11       = edgelist10 & (ix 8  <<< ix 8 <<< ix 0) .~ 0
    edgelist12       = edgelist11 & (ix 9  <<< ix 5 <<< ix 0) .~ 0
    edgelist13       = edgelist12 & (ix 9  <<< ix 6 <<< ix 0) .~ 0
    edgelist14       = edgelist13 & (ix 9  <<< ix 7 <<< ix 0) .~ 0
    edgelist15       = edgelist14 & (ix 9  <<< ix 8 <<< ix 0) .~ 0
    edgelist16       = edgelist15 & (ix 10 <<< ix 5 <<< ix 0) .~ 0
    edgelist17       = edgelist16 & (ix 10 <<< ix 6 <<< ix 0) .~ 0
    edgelist18       = edgelist17 & (ix 10 <<< ix 7 <<< ix 0) .~ 0
    edgelist19       = edgelist18 & (ix 10 <<< ix 8 <<< ix 0) .~ 0
    edgelist20       = edgelist19 & (ix 11 <<< ix 5 <<< ix 0) .~ 0
    edgelist21       = edgelist20 & (ix 11 <<< ix 6 <<< ix 0) .~ 0
    edgelist22       = edgelist21 & (ix 11 <<< ix 7 <<< ix 0) .~ 0
    edgelist23       = edgelist22 & (ix 11 <<< ix 8 <<< ix 0) .~ 0
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
      | bUppI == 5     = addToList edgelist5 a b bUpp
      | bUppI == 6     = addToList edgelist5 b c bUpp
      | bUppI == 7     = addToList edgelist5 b d bUpp
      | bUppI == 8     = addToList edgelist5 b e bUpp
      | otherwise      = error "Unexpected error in `GetEdgeList'"
  in ret

-- adds the pair u,v to edgelist
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> TpEdgelist
addToList edgelist u v degree =
  let
    a           = degree !! u
    b           = degree !! v
    eHead1      = (((edgelist ^?! ix a) ^?! ix b) ^?! ix 0)
    edgelist1_2 = edgelist    & (ix a <<< ix b <<< ix 0)            .~ (eHead1 + 1)
    edgelist1_3 = edgelist1_2 & (ix a <<< ix b <<< ix (eHead1 + 1)) .~ u
    edgelist1_4 = edgelist1_3 & (ix a <<< ix b <<< ix 0)            .~ (eHead1 + 2)
    bool1       = a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
    bool1_1     = eHead1 + 2 >= maxelist
    eHead2      = (((edgelist ^?! ix b) ^?! ix a) ^?! ix 0)
    edgelist2_2 = edgelist    & (ix b <<< ix a <<< ix 0)            .~ (eHead2 + 1)
    edgelist2_3 = edgelist2_2 & (ix b <<< ix a <<< ix (eHead2 + 1)) .~ v
    edgelist2_4 = edgelist2_3 & (ix b <<< ix a <<< ix 0)            .~ (eHead2 + 2)
    bool2       = b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)
    bool2_1     = eHead2 + 2 >= maxelist
    ret
      | bool1 &&     bool1_1 = error "More than %d entries in edgelist needed"
      | bool1 && not bool1_1 = edgelist1_4 & (ix a <<< ix b <<< ix (eHead1 + 2)) .~ v
      | bool2 &&     bool2_1 = error "More than %d entries in edgelist needed"
      | bool2 && not bool2_1 = edgelist2_4 & (ix b <<< ix a <<< ix (eHead2 + 2)) .~ u
      | otherwise            = edgelist
  in ret


subConf :: TpAxleI -> TpGoodConf -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
subConf axL@(_, axUppL) gC@(_, _, _, qXi) = do

  (gConfs, rules, deg)                                              <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- lift get

  -- 1.
  let qXi0      = head qXi
      qXi1      = qXi !! 1
      pedgeHead = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0
      loop1 i   = -- MaybeT
        if i >= pedgeHead + 1 then
          return "subConf end." -- 失敗終了
        else do
          let x = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1)
              y = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1)
          ret1 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 1 1
          ret2 <- lift . runMaybeT $ rootedSubConf axUppL gC x y 0 1
          if ret1 == Nothing && ret2 == Nothing then loop1 (i + 2) -- 再帰
          else empty            -- 正常終了
  loop1 1

rootedSubConf :: TpVertices -> TpGoodConf -> Int -> Int -> Int -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
rootedSubConf degree (qU, qV, qZ, qXi) x y clockwise j = do

  (gConfs, rules, deg)                                              <- lift ask
  (((aSLow, aSUpp, aSLev), used, image, adjmat, edgelist), posoutX) <- lift get

  -- 1.
  let used2  = replicate cartvert False
      image2 = replicate cartvert (-1) -- !!
      image3 = image & ix 0 .~ clockwise
      qZ0    = head qZ
      qZ1    = qZ !! 1
      image4 = image3 & ix qZ0 .~ x
      image5 = image4 & ix qZ1 .~ y
      used3  = used2 & ix x .~ True
      used4  = used3 & ix y .~ True

  -- 2.
  let loop2 j used image =
        let qUQ = qU !! j
        in if qUQ < 0 then
          (True, used, image, qUQ)
        else let
          qVQ      = qV    !! j
          qZQ      = qZ    !! j
          qXiQ     = qXi   !! j
          imageQUQ = image !! qUQ
          imageQVQ = image !! qVQ
          w        = if clockwise == 0 then (adjmat ^?! ix imageQVQ) ^?! ix imageQUQ
                     else                   (adjmat ^?! ix imageQUQ) ^?! ix imageQVQ
          degreeW  = degree !! w
          usedW    = used   !! w
        in if (w == -1) || (qXiQ /= 0 && qXiQ /= degreeW) || usedW then
          (False, used, image, qUQ)
        else
          loop2 (j + 1) (used & ix w .~ True) (image & ix qZQ .~ w)
      (retB, used6, image6, qUQ) = loop2 2 used4 image5
  (lift . put) (((aSLow, aSUpp, aSLev), used6, image6, adjmat, edgelist), posoutX)
  if retB then
    (liftIO . putStr) ""
  else
    empty -- 失敗終了

  -- 3. test if image is well-positioned
  let loop3 j =
        let degJ = if j == 1 then 2 * deg else deg + j - 1
            ret
              | j > deg                                              = return "loop3 end."
              | not (used !! j) && used !! (deg + j) && used !! degJ = empty -- 失敗終了
              | otherwise                                            = loop3 (j + 1)
        in ret
  loop3 1

  -- 4.
  return "rootedSubConf end." -- 正常終了



-- ##################################################################################################################
--   Hubcap
-- ##################################################################################################################
checkHubcap :: [String] -> TpAxle
  -> RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO String
checkHubcap strs ax@(axLow, axUpp, axLev) = do
  -- 1. omitted
  -- 2. omitted
  -- 3. omitted
  -- 4. omitted
  -- 5.
  (_, _, deg)     <- ask
  (rP, posoutX)   <- get
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
            ret <- runMaybeT $ checkBound (axLow !! axLev, axUpp !! axLev) (s & ix (2 * nouts) .~ 99) vi 0 0
            liftIO $ print ret
          else do
            let posX = replicate nouts xi ++ replicate nouts 0
            put (rP, posX)
            ret <- runMaybeT $ checkBound (axLow !! axLev, axUpp !! axLev) (s & ix      nouts  .~ 99) vi 0 0
            liftIO $ print ret
          loop1 (i + 1)
  loop1 0
  return "checkHubcap end."


checkBound :: TpAxleI -> [Int] -> Int -> Int -> Int
  -> MaybeT (RWST ([TpGoodConf], TpPosout, Int) () (TpReducePack, [Int]) IO) String
checkBound axL@(axLowL, axUppL) s0 maxch pos depth = do
  (gConfs, rules, deg)                                              <- lift ask
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
    if ret == Nothing then
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
                        | not (lowPosI > axLowL !! p) &&      uppPosI < axUppL !! p
                            = (axLowL                  , axUppL & ix p .~ uppPosI)
                        |      lowPosI > axLowL !! p  && not (uppPosI < axUppL !! p)
                            = (axLowL & ix p .~ lowPosI, axUppL                  )
                        | not (lowPosI > axLowL !! p) && not (uppPosI < axUppL !! p)
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
          good2 <- liftIO $ loop5_2 0
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



-- ##################################################################################################################
--   Condition
-- ##################################################################################################################
{-
checkCondition1 :: TpCond -> Int -> TpAxle -> Int -> Int -> Int -> IO Int
checkCondition1 (nn, mm) deg aA@(low, upp, lev) n m nosym =
  let ret = find (\x-> 1 <= x && x <= 2*deg) nn
  in if isJust ret then return (nosym+1) else return nosym


checkCondition2 :: TpCond -> TpAxle -> Int -> Int -> IO (TpCond, TpAxle)
checkCondition2 (nn, mm) aA@(low, upp, lev) n m =
  let low2 = low & ix (lev+1) .~ (low ^?! ix lev)
      upp2 = upp & ix (lev+1) .~ (upp ^?! ix lev)
      aLowN = (low2 ^?! ix lev) ^?! ix n
      aUppN = (upp2 ^?! ix lev) ^?! ix n
  in if m > 0     -- ★mark
    then -- new lower bound
      if aLowN >= m || m > aUppN
        then
          error "Invalid lower bound in condition"
        else
          let upp3 = upp2 & (ix lev     <<< ix n) .~ (m-1)
              low3 = low2 & (ix (lev+1) <<< ix n) .~ m
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))
    else -- new upper bound
      if aLowN > -m || -m >= aUppN
        then
          error "Invalid upper bound in condition"
        else
          let low3 = low2 & (ix lev     <<< ix n) .~ (1-m)
              upp3 = upp2 & (ix (lev+1) <<< ix n) .~ (-m)
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))
-}



