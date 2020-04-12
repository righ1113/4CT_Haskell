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
import Control.Arrow ((<<<))
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, _4, _5, _6, _7, (^.))
import Debug.Trace (trace)
import Lib (myLoop)
-}
import Control.Applicative       (empty)
import Control.Lens              ((&), (.~), ix, _1, _2, _3, _4, _5, _6, (^.))
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
maxstack   = 5              -- max height of Astack (see "Reduce")
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
reduce = return "reduce end."

{-
reduce :: TpReducePack -> TpAxle -> IO (Bool, TpAxle, [Bool], TpVertices)
reduce rP@(aStack@(aSLow, aSUpp, aSLev), _, _, _, _, _, _, _) aA@(low, upp, lev) =
  let aStack2Low = aSLow & ix 0 .~ (low ^?! ix lev)
      aStack2Upp = aSUpp & ix 0 .~ (upp ^?! ix lev)
      aStack2    = (aStack2Low, aStack2Upp, aSLev)
  in do {putStrLn "  Testing reducibility. Putting input axle on stack."; reduceSub (rP & _1 .~ aStack2) aA 1}

reduceSub :: TpReducePack -> TpAxle -> Int -> IO (Bool, TpAxle, [Bool], TpVertices)
reduceSub rP@(aStack@(aSLow, aSUpp, aSLev), bLow, bUpp, adjmat, edgelist, used, image, redquestions) aA naxles
  | naxles <= 0 = do {putStrLn "  All possibilities for lower degrees tested"; return (True, aStack, used, image)}
  | otherwise   = do
    let noconf    = 633 -- 好配置の個数
    let bLow2     = aSLow !! (naxles - 1)
    let bUpp2     = aSUpp !! (naxles - 1)
    let adjmat2   = getAdjmat (bLow2, bUpp2, adjmat)
    let edgelist2 = getEdgelist (bLow2, bUpp2, edgelist)
    -- subConfが一度もTrueを返さなかったら、Not reducible
    let f n@(retB, retH, _, _, _) x cont
          | retB      = n
          | otherwise = cont $ subConf adjmat2 bUpp2 (redquestions !! x) edgelist2 (n & _2 .~ (retH+1))
    let (retB, retH, used', image', _) = myLoop f id (False, 0, used, image, 0) [0..(noconf-1)]
    if retB
      then do
        putStrLn "    pass subConf."
        -- Semi-reducibility test found h-th configuration, say K, appearing
        let redverts = ((redquestions !! retH) ^. _1) !! 1
        let redring  = ((redquestions !! retH) ^. _2) !! 1
        -- the above are no vertices and ring-size of free completion of K
        -- could not use conf[h][0][0], because conf may be NULL
        let (retN, aStack') = getReduceN (redring + 1) redverts (naxles - 1) image' aStack bLow2 bUpp2
        reduceSub (((((rP & _1 .~ aStack') & _4 .~ adjmat2) & _5 .~ edgelist2) & _6 .~ used') & _7 .~ image') aA retN
      else do
        putStrLn "    Not reducible"
        return (False, aStack, used', image')


getAdjmat :: (TpVertices, TpVertices, TpAdjmat) -> TpAdjmat
getAdjmat (bLow, bUpp, _) =
  let deg        = head bLow
      adjmat     = replicate cartvert $ replicate cartvert (-1)
      f n x cont = cont $ getAdjmatSub deg bUpp n x
  in myLoop f id adjmat [1..deg]

data Way = Forward | Backward deriving Eq
chgAdjmat :: TpAdjmat -> Int -> Int -> Int -> Way -> TpAdjmat
chgAdjmat adjmat a b c way =
  if way == Forward
    then let
      adjmat2 = adjmat  & (ix a <<< ix b) .~ c
      adjmat3 = adjmat2 & (ix c <<< ix a) .~ b
      adjmat4 = adjmat3 & (ix b <<< ix c) .~ a
        in
          adjmat4
    else let
      adjmat2 = adjmat  & (ix a <<< ix b) .~ c
      adjmat3 = adjmat2 & (ix b <<< ix c) .~ a
      adjmat4 = adjmat3 & (ix c <<< ix a) .~ b
        in
          adjmat4

getAdjmatSub :: Int -> [Int] -> TpAdjmat -> Int -> TpAdjmat
getAdjmatSub deg bUpp adjmat i =
  let h       = if i == 1 then deg else i - 1
      a       = deg + h
      adjmat2 = chgAdjmat adjmat 0 h i Forward
      adjmat3 = chgAdjmat adjmat2 i h a Forward
  in if bUpp !! i < 9
    then
      doFan deg i (bUpp !! i) adjmat3
    else
      adjmat3

doFan :: Int -> Int -> Int -> TpAdjmat -> TpAdjmat
doFan deg i k adjmat =
  let a = if i == 1 then 2 * deg else deg + i - 1
      b = deg + i
  in if k == 5    -- ★mark
    then
      chgAdjmat adjmat i a b Backward
    else let
      c = 2 * deg + i
      adjmat2 = chgAdjmat adjmat i a c Backward
        in if k == 6
          then
            chgAdjmat adjmat2 i c b Backward
          else let
            d = 3 * deg + i
            adjmat3 = chgAdjmat adjmat2 i c d Backward
              in if k == 7
                then
                  chgAdjmat adjmat3 i d b Backward
                else let
                  e = 4 * deg + i
                  adjmat4 = chgAdjmat adjmat3 i d e Backward
                  adjmat5 = chgAdjmat adjmat4 i e b Backward
                    in
                      adjmat5


getEdgelist :: (TpVertices, TpVertices, TpEdgelist) -> TpEdgelist
getEdgelist (bLow, bUpp, edgelist) =
  let
    edgelist2  =  edgelist  & (ix 5  <<< ix 5 <<< ix 0) .~ 0
    edgelist3  =  edgelist2 & (ix 6  <<< ix 5 <<< ix 0) .~ 0
    edgelist4  =  edgelist3 & (ix 6  <<< ix 6 <<< ix 0) .~ 0
    edgelist5  =  edgelist4 & (ix 7  <<< ix 5 <<< ix 0) .~ 0
    edgelist6  =  edgelist5 & (ix 7  <<< ix 6 <<< ix 0) .~ 0
    edgelist7  =  edgelist6 & (ix 7  <<< ix 7 <<< ix 0) .~ 0
    edgelist8  =  edgelist7 & (ix 8  <<< ix 5 <<< ix 0) .~ 0
    edgelist9  =  edgelist8 & (ix 8  <<< ix 6 <<< ix 0) .~ 0
    edgelist10 =  edgelist9 & (ix 8  <<< ix 7 <<< ix 0) .~ 0
    edgelist11 = edgelist10 & (ix 8  <<< ix 8 <<< ix 0) .~ 0
    edgelist12 = edgelist11 & (ix 9  <<< ix 5 <<< ix 0) .~ 0
    edgelist13 = edgelist12 & (ix 9  <<< ix 6 <<< ix 0) .~ 0
    edgelist14 = edgelist13 & (ix 9  <<< ix 7 <<< ix 0) .~ 0
    edgelist15 = edgelist14 & (ix 9  <<< ix 8 <<< ix 0) .~ 0
    edgelist16 = edgelist15 & (ix 10 <<< ix 5 <<< ix 0) .~ 0
    edgelist17 = edgelist16 & (ix 10 <<< ix 6 <<< ix 0) .~ 0
    edgelist18 = edgelist17 & (ix 10 <<< ix 7 <<< ix 0) .~ 0
    edgelist19 = edgelist18 & (ix 10 <<< ix 8 <<< ix 0) .~ 0
    edgelist20 = edgelist19 & (ix 11 <<< ix 5 <<< ix 0) .~ 0
    edgelist21 = edgelist20 & (ix 11 <<< ix 6 <<< ix 0) .~ 0
    edgelist22 = edgelist21 & (ix 11 <<< ix 7 <<< ix 0) .~ 0
    edgelist23 = edgelist22 & (ix 11 <<< ix 8 <<< ix 0) .~ 0
    deg        = head bUpp
    f n x cont = cont $ getEdgelistSub bLow bUpp n deg x
  in myLoop f id edgelist23 [1..deg]

getEdgelistSub :: TpVertices -> TpVertices -> TpEdgelist -> Int -> Int -> TpEdgelist
getEdgelistSub bLow bUpp edgelist deg i =
  let
    edgelist2 = addToList edgelist 0 i bUpp
    h = if i == 1 then deg else i - 1
    edgelist3 = addToList edgelist2 i h bUpp
    a = deg + h
    b = deg + i
    edgelist4 = addToList edgelist3 i a bUpp
    edgelist5 = addToList edgelist4 i b bUpp
    bLowI = bLow !! i
    bUppI = bUpp !! i
  in if bLowI /= bUppI    -- ★mark
    then
      edgelist5
    else
      -- in this case we are not interested in the fan edges
      if bUppI == 5
        then
          addToList edgelist5 a b bUpp
        else let
          c = 2 * deg + i
          edgelist6 = addToList edgelist5 a c bUpp
          edgelist7 = addToList edgelist6 i c bUpp
            in if bUppI == 6
              then
                addToList edgelist7 b c bUpp
              else let
                d = 3 * deg + i;
                edgelist8 = addToList edgelist7 c d bUpp
                edgelist9 = addToList edgelist8 i d bUpp
                  in if bUppI == 7
                    then
                      addToList edgelist9 b d bUpp
                    else
                      if bUppI == 8
                        then let
                          e = 4 * deg + i
                          edgelist10 = addToList edgelist9 d e bUpp
                          edgelist11 = addToList edgelist10 i e bUpp
                            in
                              addToList edgelist11 b e bUpp
                        else
                          error "Unexpected error in `GetEdgeList'"

-- adds the pair u,v to edgelist
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> TpEdgelist
addToList edgelist u v degree =
  let a = degree !! u
      b = degree !! v
  in if a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)   -- ★mark
    then let
      eHead  = (((edgelist ^?! ix a) ^?! ix b) ^?! ix 0)
        in if eHead + 2 >= maxelist
          then
            error "More than %d entries in edgelist needed"
          else let
            edgelist2 = edgelist  & (ix a <<< ix b <<< ix 0)           .~ (eHead + 1)
            edgelist3 = edgelist2 & (ix a <<< ix b <<< ix (eHead + 1)) .~ u
            edgelist4 = edgelist3 & (ix a <<< ix b <<< ix 0)           .~ (eHead + 2)
              in
                edgelist4 & (ix a <<< ix b <<< ix (eHead + 2)) .~ v
    else
      if b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)
        then let
          eHead  = (((edgelist ^?! ix b) ^?! ix a) ^?! ix 0)
            in if eHead + 2 >= maxelist
              then
                error "More than %d entries in edgelist needed"
              else let
                edgelist2 = edgelist  & (ix b <<< ix a <<< ix 0)           .~ (eHead + 1)
                edgelist3 = edgelist2 & (ix b <<< ix a <<< ix (eHead + 1)) .~ v
                edgelist4 = edgelist3 & (ix b <<< ix a <<< ix 0)           .~ (eHead + 2)
                  in
                    edgelist4 & (ix b <<< ix a <<< ix (eHead + 2)) .~ u
        else
          edgelist


subConf :: TpAdjmat -> TpVertices -> TpGoodConf -> TpEdgelist -> TpConfPack -> TpConfPack
subConf adjmat degree question@(_, _, _, qXi) edgelist confPack =
  let qXi0 = head qXi
      qXi1 = qXi !! 1
      pedgeHead = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0
  in trace ("    (qXi0,qXi1,pedgeHead) = " ++ show (qXi0,qXi1,pedgeHead)) $ subConf' adjmat degree question edgelist confPack pedgeHead 1

subConf' :: TpAdjmat -> TpVertices -> TpGoodConf -> TpEdgelist -> TpConfPack -> Int -> Int -> TpConfPack
subConf' adjmat degree question@(qU, qV, qZ, qXi) edgelist (retB, retH, used, image, _) pedgeHead i
  | i == pedgeHead + 1 = (False, retH, used, image, 0)
  | otherwise          =
    let qXi0                          = head qXi
        qXi1                          = qXi !! 1
        x                             = (((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1))
        y                             = (((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix i)
        headD                         = head degree
        (retB1, retH1, used1, image1, _) = rootedSubConf degree headD adjmat question x y 1 1 (retB,  retH,  used,  image , 0)
        (retB2, retH2, used2, image2, _) = rootedSubConf degree headD adjmat question x y 0 1 (retB1, retH1, used1, image1, 0)
    in if retB1
      then
        (True, retH, used1, image1, 0)
      else if retB2
        then
          (True, retH, used2, image2, 0)
        else
          subConf' adjmat degree question edgelist (retB2, retH, used2, image2, 0) pedgeHead (i+1)

rootedSubConf :: TpVertices -> Int -> TpAdjmat -> TpGoodConf -> Int -> Int -> Int -> Int -> TpConfPack -> TpConfPack
rootedSubConf degree deg adjmat question@(qU, qV, qZ, qXi) x y clockwise j (retB, retH, used, image, _) = -- (True,  retH, used, image, 0)
  let used2  = replicate cartvert False
      image2 = replicate cartvert (-1) -- !!
      image3 = image & ix 0 .~ clockwise
      qZ0 = head qZ
      qZ1 = qZ !! 1
      image4 = image3 & ix qZ0 .~ x
      image5 = image4 & ix qZ1 .~ y
      used3 = used2 & ix x .~ True
      used4 = used3 & ix y .~ True

      f n@(retB, _, _, _, qUQ) x cont
        | qUQ < 0   = n
        | not retB  = n
        | otherwise = cont $ rootedSubConfSub1 adjmat degree question clockwise n x
      retPack@(retB', retH', used', image', _) = myLoop f id (True, retH, used4, image5, 0) [2, 3 .. 26]
  in if not retB'
    then
      retPack
    else
      -- test if image is well-positioned
      let f2 n x cont
            | n         = n
            | otherwise = cont $ rootedSubConfSub2 used' deg x
      in if myLoop f2 id False [1, 2 .. deg]
        then
          (False, retH', used', image', 0)
        else
          (True,  retH', used', image', 0)

rootedSubConfSub1 :: TpAdjmat -> TpVertices -> TpGoodConf -> Int -> TpConfPack -> Int -> TpConfPack
rootedSubConfSub1 adjmat degree question@(qU, qV, qZ, qXi) clockwise (_, retH, used, image, _) q =
  let qUQ      = qU !! q
  in if qUQ < 0
    then (True,  retH, used, image, qUQ)
    else let
      qVQ      = qV !! q
      qZQ      = qZ !! q
      qXiQ     = qXi !! q
      imageQUQ = image !! qUQ
      imageQVQ = image !! qVQ
      w        = if clockwise == 0
                  then (adjmat ^?! ix imageQVQ) ^?! ix imageQUQ
                  else (adjmat ^?! ix imageQUQ) ^?! ix imageQVQ
      degreeW  = degree !! w
      usedW    = used !! w
--      in trace ("(qUQ,w,qXiQ,degreeW,usedW) = " ++ show (qUQ,w,qXiQ,degreeW,usedW)) $ if (w == -1) || (qXiQ /= 0 && qXiQ /= degreeW) || usedW
      in trace ("    (qUQ,w,qXiQ,degreeW,usedW) = " ++ show (qUQ,w,qXiQ,degreeW,usedW)) $ if (w == -1) || usedW
        then (False, retH, used,                image,               qUQ)
        else (True,  retH, used & ix w .~ True, image & ix qZQ .~ w, qUQ)

rootedSubConfSub2 :: [Bool] -> Int -> Int -> Bool
rootedSubConfSub2 used deg j =
  let cc    = if j == 1 then 2 * deg else deg + j - 1;
      usedJ = used !! j;
      usedD = used !! (deg + j);
      usedC = used !! cc
  in not usedJ && usedD && usedC


getReduceN :: Int -> Int -> Int -> TpVertices -> TpAxle -> [Int] -> [Int] -> (Int, TpAxle)
getReduceN i redverts naxles image aStack@(aSLow, aSUpp, aSLev) bLow bUpp
  | i > redverts = (naxles, aStack)
  | otherwise    =
    let v = image !! i
    in if bLow !! v == bUpp !! v
      then
        getReduceN (i + 1) redverts naxles image aStack bLow bUpp
      else
        if naxles >= maxstack
          then
            error "More than %d elements in axle stack needed"
          else
            let aStack2Low = aSLow & ix naxles .~ bLow
                aStack2Upp = aSUpp & ix naxles .~ bUpp
                aStack3Upp = aStack2Upp & (ix naxles <<< ix v) .~ (bUpp ^?! ix v - 1)
                aStack2    = (aStack2Low, aStack3Upp, aSLev)
            in getReduceN (i + 1) redverts (naxles + 1) image aStack2 bLow bUpp
-}
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



