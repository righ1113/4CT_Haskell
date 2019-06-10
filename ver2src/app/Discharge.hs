{-
◆author: righ1113
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「present7」「rules」「unavoidable.conf」を取得し、
    ver2src/readFile/に置く。
2. > stack run discharge-exe
3. > 「7」を入力してEnter。
4. 中心の次数7のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK（今は表示されない）
-}
module Main where
-- module Discharge where

import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Control.Arrow ((<<<))
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, _4, _5, _6, _7, (^.))
import Debug.Trace (trace)
import Lib (myLoop)


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

type TpAxle       = ([[Int]], [[Int]], Int)
type TpAxleI      = ([Int], [Int])
type TpCond       = ([Int], [Int])
type TpAdjmat     = [[Int]]
type TpVertices   = [Int]
type TpQuestion   = ([Int], [Int], [Int], [Int])
type TpEdgelist   = [[[Int]]]
type TpPosout     = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]], [Int])
type TpPosoutI    = (Int, Int, Int, [Int], [Int], [Int], Int)
type TpReducePack = (TpAxle, [Int], [Int], TpAdjmat, TpEdgelist, [Bool], TpVertices, [TpQuestion])
type TpConfPack   = (Bool, Int, [Bool], TpVertices, Int)


main :: IO ()
main = do
  putStrLn "これは四色定理の放電法をおこなうプログラムです"
  putStrLn "中心の次数7,8,9,10,11のいずれかを入力してください"
  degStr <- getLine
  let deg = read degStr :: Int

  -- TpAxle
  let axles0 = replicate maxlev $ replicate cartvert 0
  let axlesLow = take cartvert ([deg] ++ replicate (5*deg) 5     ++ repeat 0) : axles0
  let axlesUpp = take cartvert ([deg] ++ replicate (5*deg) infty ++ repeat 0) : axles0

  -- TpCond
  let nn = replicate maxlev 0
  let mm = replicate maxlev 0

  -- TpOutlet & TpPosout
{-
  let number  = replicate (2 * maxoutlets) 0
  let nolines = replicate (2 * maxoutlets) 0
  let value   = replicate (2 * maxoutlets) 0
  let pos     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let low     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let upp     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let xx      = replicate (2 * maxoutlets) 0
-}
  posoutStr    <- readFile $ "readFile/rules" ++ degStr ++ "HS.txt"
  let posout   = read posoutStr :: TpPosout -- CheckHubcap(axles, NULL, 0, print); -- read rules, compute outlets

  -- TpReducePack
  let aSLow    = replicate (maxlev + 1) $ replicate cartvert 0
  let aSUpp    = replicate (maxlev + 1) $ replicate cartvert 0
  let bLow     = replicate cartvert 0
  let bUpp     = replicate cartvert 0
  let adjmat   = replicate cartvert $ replicate cartvert 0
  let edgelist = replicate 12 $ replicate 9 $ replicate maxelist 0
  let used     = replicate cartvert False
  let image    = replicate cartvert 0
  let qU       = replicate verts 0
  let qV       = replicate verts 0
  let qZ       = replicate verts 0
  let qXi      = replicate verts 0
  redQStr      <- readFile "readFile/unavoidableHS.txt"
  let redQ     = read redQStr :: [TpQuestion] -- (void) Reduce(NULL, 0, 0); -- read unavoidable set

  inStr <- readFile $ "readFile/present" ++ degStr
  ret   <- mainLoop ((aSLow, aSUpp, 0), bLow, bUpp, adjmat, edgelist, used, image, redQ)
                    posout
                    (nn, mm)
                    deg
                    0
                    (axlesLow, axlesUpp, 0)
                    (tail (map words (lines inStr)))

  -- final check
  if ret == "Q.E.D." then
    putStrLn $ "中心の次数" ++ degStr ++ "のグラフは、電荷が負になるか、近くに好配置があらわれるかです"
  else
    putStr ""
  putStrLn "プログラムは正常終了しました"


mainLoop :: TpReducePack -> TpPosout -> TpCond -> Int -> Int -> TpAxle -> [[String]] -> IO String
mainLoop rP posout (nn, mm) deg nosym axles@(low, upp, lev) tactics
  | lev >= maxlev = error "More than %d levels"
  | lev < 0       = return $ head $ head tactics
  | otherwise =
    case head tactics !! 1 of
      "S" -> do
              putStrLn "Symmetry"
              checkSymmetry (tail (tail (head tactics))) axles posout nosym
              mainLoop rP posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "R" -> do
              putStrLn "Reduce"
              (retB, aStack', used', image') <- reduce rP axles
              if retB
                then
                  mainLoop (((rP & _1 .~ aStack') & _6 .~ used') & _7 .~ image') posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
                else
                  error "Reducibility failed"
      "H" -> do
              putStrLn "Hubcap"
              posout' <- checkHubcap posout (tail (tail (head tactics))) (low, upp, lev) deg
              mainLoop rP posout' (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "C" -> do
              putStrLn "Condition"
              let n = read (head tactics !! 2) :: Int
              let m = read (head tactics !! 3) :: Int
              nosym2                   <- checkCondition1 (nn, mm) deg axles n m nosym
              (cond2, (low2, upp2, _)) <- checkCondition2 (nn, mm) axles n m
              mainLoop rP posout cond2 deg nosym2 (low2, upp2, lev+1) (tail tactics)
      _   -> error "Invalid instruction"


checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, upp, lev) posout@(number, nolines, value, pos, plow, pupp, xx) nosym =
  let [k, epsilon, level, line] = map read str :: [Int]
      i                         = fromJust $ find (==line) number
      pI                        = (number !! i, nolines !! i, value !! i, pos !! i, plow !! i, pupp !! i, xx !! i)
  in if k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1 then                       error "Illegal symmetry"
     else if i >= nosym then                                                                    error "No symmetry as requested"
          else if nolines !! i /= level + 1 then                                                error "Level mismatch"
               else if epsilon == 0 && outletForced (low !! lev, upp !! lev) pI (k+1) /= 1 then error "Invalid symmetry"
                    else if reflForced aA /= 1 then                                             error "Invalid reflected symmetry"
                                               else                                             putStrLn "  checkSymmetry OK."

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int
outletForced aAI@(lowI, uppI) (numberI, nolinesI, valueI, posI, plowI, puppI, xxI) y =
  let deg = lowI !! 0
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
  in if m > 0
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
  in if k == 5
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
  in if bLowI /= bUppI
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
  in if a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
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


subConf :: TpAdjmat -> TpVertices -> TpQuestion -> TpEdgelist -> TpConfPack -> TpConfPack
subConf adjmat degree question@(_, _, _, qXi) edgelist confPack =
  let qXi0 = head qXi
      qXi1 = qXi !! 1
      pedgeHead = ((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0
  in trace ("    (qXi0,qXi1,pedgeHead) = " ++ show (qXi0,qXi1,pedgeHead)) $ subConf' adjmat degree question edgelist confPack pedgeHead 1

subConf' :: TpAdjmat -> TpVertices -> TpQuestion -> TpEdgelist -> TpConfPack -> Int -> Int -> TpConfPack
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

rootedSubConf :: TpVertices -> Int -> TpAdjmat -> TpQuestion -> Int -> Int -> Int -> Int -> TpConfPack -> TpConfPack
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

rootedSubConfSub1 :: TpAdjmat -> TpVertices -> TpQuestion -> Int -> TpConfPack -> Int -> TpConfPack
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


checkHubcap :: TpPosout -> [String] -> TpAxle -> Int -> IO TpPosout
checkHubcap posout strs aA deg =
  let f n x cont
        | otherwise = cont $ checkHubcapSub n strs aA deg x
  in return $ myLoop f id posout [0, 1 .. length strs - 1]

checkHubcapSub :: TpPosout -> [String] -> TpAxle -> Int -> Int -> TpPosout
checkHubcapSub posout@(_, _, _, _, _, _, pxx) strs aA@(low, upp, lev) deg i = -- posout
  let xyvs         = map read strs :: [(Int, Int, Int)]
      s            = replicate (2 * maxoutlets + 1) 0
      nouts        = difNouts !! deg
      s2           = s & ix (2 * nouts) .~ 99 -- to indicate end of list
      (xi, yi, vi) = xyvs !! i
      pxx2         = replicate nouts 0 ++ drop nouts pxx
  in if xi /= yi
    then let
      pxx3 = take nouts pxx2 ++ replicate nouts yi
      in checkBound (low !! lev, upp !! lev) (posout & _7 .~ pxx3) nouts s2 vi 0 0
    else
         checkBound (low !! lev, upp !! lev) (posout & _7 .~ pxx2) nouts s2 vi 0 0

checkBound :: TpAxleI -> TpPosout -> Int -> [Int] -> Int -> Int -> Int -> TpPosout
checkBound aAI posout@(_, _, value, _, _, _, pxx) nouts s maxch pos depth =
  -- compute forced and permitted rules, allowedch, forcedch, update s
  let forcedch       = 0
      allowedch      = 0

      f n@(si, _) x cont
        | si >= 99   = n
        | otherwise  = cont $ checkBoundSub1 s value n x
      (_, forcedch2) = myLoop f id (0, forcedch) [0, 1 .. (2 * nouts + 1)]

  -- check if inequality holds
  in if forcedch2 + allowedch <= maxch
    then
      trace ("  " ++ show depth ++ " Inequality holds. Case done.") posout
    else let

      f2 n@(retB, sPos, _, _, _, _, _) x cont
        | sPos >= 99 = n
        | retB       = n
        | otherwise  = cont $ checkBoundSub2 value pos n x
      (retB, _, _, allowedch2, s2, _, posout2) = myLoop f2 id (False, 0, forcedch2, allowedch, s, aAI, posout) [0, 1 .. (2 * nouts + 1)]
        in if retB
          then
            trace "  Inequality holds." posout2
          else
            error "Unexpected error 101"

checkBoundSub1 :: [Int] -> [Int] -> (Int, Int) -> Int -> (Int, Int)
checkBoundSub1 s value (_, fo) i =
  let si     = s     !! i
      valuei = value !! i
  in if si > 0
    then (si, fo + valuei)
    else (si, fo         )

checkBoundSub2 :: [Int] -> Int -> (Bool, Int, Int, Int, [Int], TpAxleI, TpPosout) -> Int -> (Bool, Int, Int, Int, [Int], TpAxleI, TpPosout)
checkBoundSub2 value pos (_, _, fo, al, s, aAI, posout@(_, _, _, _, _, _, xx)) k =
  let sPos   = s     !! pos
      valuek = value !! k
  in if sPos == 0 && valuek >= 0
    then let
      y = xx !! k
      {- x = PO->x;
      -- accepting positioned outlet PO, computing AA
      CopyAxle(AA, A);
      for (i = 0; i < PO->T->nolines; ++i) {
        p = PO->T->pos[i];
        p = x - 1 + (p - 1) % deg < deg ? p + x - 1 : p + x - 1 - deg;
        if (PO->T->low[i] > AA->low[p])
          AA->low[p] = PO->T->low[i];
        if (PO->T->upp[i] < AA->upp[p])
          AA->upp[p] = PO->T->upp[i];
        if (AA->low[p] > AA->upp[p])
          error "Unexpected error 321"
      }	/* i */ -}

      -- Check if a previously rejected positioned outlet is forced to apply
      f n x cont
        | n == 0     = n
        | otherwise  = cont $ checkBoundSub2Sub aAI s posout n x
      good = myLoop f id 1 [0, 1 .. (pos - 1)]
        in if good /= 0
          then (True, sPos, fo, al, s, aAI, posout)
            {- -- recursion with PO forced
            for (i = 0; (sprime[i] = s[i]) < 99; ++i) -- sprimeにコピーしている
            sprime[pos] = 1;
            CheckBound(AA, posout, sprime, maxch, pos + 1, depth + 1, lineno, print); -}
          else
            trace "    Rejecting positioned outlet " (True, sPos, fo, al + valuek, s & ix pos .~ (-1), aAI, posout)
    else (False, sPos, fo, al, s, aAI, posout)

checkBoundSub2Sub :: TpAxleI -> [Int] -> TpPosout -> Int -> Int -> Int
checkBoundSub2Sub aAI s posout@(number, nolines, value, pos, low, upp, xx) _ i =
  if (s !! i == -1) && (outletForced aAI (number !! i, nolines !! i, value !! i, pos !! i, low !! i, upp !! i, xx !! i) (xx !! i) /= 0)
    then 0
    else 1



