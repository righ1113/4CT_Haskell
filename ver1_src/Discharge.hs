{-
◆author: righ1113
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「present7」「rules」「unavoidable.conf」を取得し、
    本ファイル、「rules7HS.txt」「unavoidableHS.txt」と同じ場所に置く。
2. > stack install lens --resolver lts
3. > stack exec ghci --resolver lts
4. > :l Discharge
5. > main
6. > 「7」を入力してEnter。
7. 中心の次数7のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK
-}
module Discharge where

import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Control.Arrow ((<<<))
import Control.Lens ((&), (.~), ix, (^?!), _1, _2, _6, _7, (^.))


verts      = 27             -- max number of vertices in a free completion + 1

confs      = 640            -- max number of configurations
maxval     = 12
cartvert   = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

infty      = 12             -- the "12" in the definition of limited part
maxoutlets = 110            -- max number of outlets

maxelist   = 134            -- length of edgelist[a][b]

maxstack   = 5              -- max height of Astack (see "Reduce")
maxlev     = 12             -- max level of an input line + 1

type TpAxle       = ([[Int]], [[Int]], Int)
type TpCond       = ([Int], [Int])
type TpAdjmat     = [[Int]]
type TpVertices   = [Int]
type TpQuestion   = ([Int], [Int], [Int], [Int])
type TpEdgelist   = [[[Int]]]
type TpPosout     = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]], [Int])
type TpReducePack = (TpAxle, [Int], [Int], TpAdjmat, TpEdgelist, [Bool], TpVertices, [TpQuestion])
type TpConfPack   = (Bool, Int, [Bool], TpVertices)


main :: IO ()
main = do
  putStrLn "これは四色定理の放電法をおこなうプログラムです"
  putStrLn "中心の次数7,8,9,10,11のいずれかを入力してください"
  degStr <- getLine
  let deg = read degStr :: Int

  -- TpAxle
  let axles0 = replicate maxlev $ replicate cartvert 0
  let axlesLow = take (maxlev + 1) ([deg] ++ replicate (5*deg) 5     ++ repeat 0) : axles0
  let axlesUpp = take (maxlev + 1) ([deg] ++ replicate (5*deg) infty ++ repeat 0) : axles0

  -- TpCond
  let nn = replicate maxlev 0
  let mm = replicate maxlev 0

  -- CheckHubcap(axles, NULL, 0, print); -- read rules, compute outlets
{-
  let number  = replicate (2 * maxoutlets) 0
  let nolines = replicate (2 * maxoutlets) 0
  let value   = replicate (2 * maxoutlets) 0
  let pos     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let low     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let upp     = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  let xx      = replicate (2 * maxoutlets) 0
-}
  -- TpOutlet & TpPosout
  posoutStr    <- readFile $ "rules" ++ degStr ++ "HS.txt"
  let posout   = read posoutStr :: TpPosout

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
  redQStr      <- readFile "unavoidableHS.txt"
  let redQ     = read redQStr :: [TpQuestion] -- (void) Reduce(NULL, 0, 0); -- read unavoidable set


  inStr <- readFile $ "present" ++ degStr
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
              -- checkHubcap posout (tail (tail (head tactics))) (low, upp, lev)
              mainLoop rP posout (nn, mm) deg nosym (low, upp, lev-1) (tail tactics)
      "C" -> do
              putStrLn "Condition"
              let n = read (head tactics !! 2) :: Int
              let m = read (head tactics !! 3) :: Int
              nosym2                   <- checkCondition1 (nn, mm) deg axles n m nosym
              (cond2, (low2, upp2, _)) <- checkCondition2 (nn, mm) axles n m
              mainLoop rP posout cond2 deg nosym2 (low2, upp2, lev+1) (tail tactics)
      _   -> error "Invalid instruction"


checkSymmetry :: [String] -> TpAxle -> TpPosout -> Int -> IO ()
checkSymmetry str aA@(low, _, lev) posout@(number, nolines, _, _, _, _, _) nosym =
  let [k, epsilon, level, line] = map read str :: [Int];
      i = fromJust $ find (==line) number
  in if k < 0 || k > head (low !! lev) || epsilon < 0 || epsilon > 1 then error "Illegal symmetry"
     else if i >= nosym then                                              error "No symmetry as requested"
          else if nolines !! i /= level + 1 then                          error "Level mismatch"
               else if epsilon == 0 && outletForced aA /= 1 then          error "Invalid symmetry"
                    else if reflForced aA /= 1 then                       error "Invalid reflected symmetry"
                                               else                       putStrLn "checkSymmetry OK."


outletForced :: TpAxle -> Int
outletForced _ = 1


reflForced :: TpAxle -> Int
reflForced _ = 1


checkCondition1 :: TpCond -> Int -> TpAxle -> Int -> Int -> Int -> IO Int
checkCondition1 (nn, mm) deg aA@(low, upp, lev) n m nosym =
  let ret = find (\x-> 1 <= x && x <= 2*deg) nn
  in if isJust ret then return (nosym+1) else return nosym


checkCondition2 :: TpCond -> TpAxle -> Int -> Int -> IO (TpCond, TpAxle)
checkCondition2 (nn, mm) aA@(low, upp, lev) n m =
  let low2 = low & ix (lev+1) .~ (low ^?! ix lev);
      upp2 = upp & ix (lev+1) .~ (upp ^?! ix lev);
      aLowN = (low2 ^?! ix lev) ^?! ix n;
      aUppN = (upp2 ^?! ix lev) ^?! ix n
  in if m > 0
    then -- new lower bound
      if aLowN >= m || m > aUppN
        then
          error "Invalid lower bound in condition"
        else
          let upp3 = upp2 & (ix lev     <<< ix n) .~ (m-1);
              low3 = low2 & (ix (lev+1) <<< ix n) .~ m;
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0;
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))
    else -- new upper bound
      if aLowN > -m || -m >= aUppN
        then
          error "Invalid upper bound in condition"
        else
          let low3 = low2 & (ix lev     <<< ix n) .~ (1-m);
              upp3 = upp2 & (ix (lev+1) <<< ix n) .~ (-m);
              nnn = (nn & ix lev .~ n) & ix (lev+1) .~ 0;
              mmm = (mm & ix lev .~ m) & ix (lev+1) .~ 0
          in return ((nnn, mmm), (low3, upp3, lev))


-- 参考記事：https://mkotha.hatenadiary.org/entry/20110430/1304122048
myLoop :: (accT -> a -> (accT -> b) -> b) -> (accT -> b) -> accT -> [a] -> b
myLoop _ g acc []     = g acc
myLoop f g acc (x:xs) = f acc x $ \acc' -> myLoop f g acc' xs


reduce :: TpReducePack -> TpAxle -> IO (Bool, TpAxle, [Bool], TpVertices)
reduce rP@(aStack@(aSLow, aSUpp, aSLev), _, _, _, _, _, _, _) aA@(low, upp, lev) =
  let aStack2Low = aSLow & ix 0 .~ (low ^?! ix lev);
      aStack2Upp = aSUpp & ix 0 .~ (upp ^?! ix lev);
      aStack2    = (aStack2Low, aStack2Upp, aSLev)
  in do {putStrLn "Testing reducibility. Putting input axle on stack."; reduceSub (rP & _1 .~ aStack2) aA 1}
reduceSub :: TpReducePack -> TpAxle -> Int -> IO (Bool, TpAxle, [Bool], TpVertices)
reduceSub rP@(aStack@(aSLow, aSUpp, aSLev), bLow, bUpp, adjmat, edgelist, used, image, redquestions) aA naxles
  | naxles <= 0 = do {putStrLn "All possibilities for lower degrees tested"; return (True, aStack, used, image)}
  | otherwise   = do
    let noconf = 633 -- 好配置の個数
    let bLow2 = aSLow !! (naxles - 1)
    let bUpp2 = aSUpp !! (naxles - 1)
    let (bLow3, bUpp3) = getadjmat   (bLow2, bUpp2) adjmat
    let (bLow4, bUpp4) = getEdgelist (bLow3, bUpp3) edgelist

    -- subConfが一度もTrueを返さなかったら、Not reducible
    let f n@(retB, retH, _, _) x cont
          | retB      = n
          | otherwise = cont $ subConf adjmat bUpp4 edgelist 1 (n & _2 .~ (retH+1)) x
    let (retB, retH, used', image') = myLoop f id (False, 0, used, image) [redquestions !! h | h <- [0..(noconf-1)]]
    if retB
      then do
        -- Semi-reducibility test found h-th configuration, say K, appearing
        let redverts = ((redquestions !! retH) ^. _1) !! 1
        let redring  = ((redquestions !! retH) ^. _2) !! 1
        -- the above are no vertices and ring-size of free completion of K
        -- could not use conf[h][0][0], because conf may be NULL
        let (retN, aStack') = getReduceN (redring + 1) redverts (naxles - 1) image' aStack bLow4 bUpp4
        reduceSub (((rP & _1 .~ aStack') & _6 .~ used') & _7 .~ image') aA retN
      else do
        putStrLn "Not reducible"
        return (False, aStack, used', image')


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
            let aStack2Low = aSLow & ix naxles .~ bLow;
                aStack2Upp = aSUpp & ix naxles .~ bUpp;
                aStack3Upp = aStack2Upp & (ix naxles <<< ix v) .~ (bUpp ^?! ix v - 1);
                aStack2    = (aStack2Low, aStack3Upp, aSLev)
            in getReduceN (i + 1) redverts (naxles + 1) image aStack2 bLow bUpp


getadjmat :: ([Int], [Int]) -> TpAdjmat -> ([Int], [Int])
getadjmat b adjmat = b
getEdgelist :: ([Int], [Int]) -> TpEdgelist -> ([Int], [Int])
getEdgelist b edgelist = b


--    static int used[cartvert]
subConf :: TpAdjmat -> TpVertices -> TpEdgelist -> Int -> TpConfPack -> TpQuestion -> TpConfPack
subConf adjmat degree edgelist i (retB, retH, used, image) question@(_, _, _, qXi) =
  let qXi0 = head qXi;
      qXi1 = qXi !! 1
  in subConf' adjmat degree edgelist (((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix 0) i (retB, retH, used, image) question
subConf' :: TpAdjmat -> TpVertices -> TpEdgelist -> Int -> Int -> TpConfPack -> TpQuestion -> TpConfPack
subConf' adjmat degree edgelist pedgeHead i (retB, retH, used, image) question@(qU, qV, qZ, qXi)
  | i == pedgeHead + 1 = (False, retH, used, image)
  | otherwise          =
    let qXi0                          = head qXi;
        qXi1                          = qXi !! 1;
        x                             = (((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix (i + 1));
        y                             = (((edgelist ^?! ix qXi0) ^?! ix qXi1) ^?! ix i);
        headD                         = head degree;
        (retB1, retH1, used1, image1) = rootedSubConf degree headD adjmat question x y 1 1 (retB,  retH,  used,  image );
        (retB2, retH2, used2, image2) = rootedSubConf degree headD adjmat question x y 0 1 (retB1, retH1, used1, image1)
    in if retB1 || retB2
      then
        (True, retH2, used2, image2)
      else
        subConf' adjmat degree edgelist pedgeHead (i+1) (retB2, retH2, used2, image2) question


rootedSubConf :: TpVertices -> Int -> TpAdjmat -> TpQuestion -> Int -> Int -> Int -> Int -> TpConfPack -> TpConfPack
rootedSubConf degree deg adjmat question@(qU, qV, qZ, qXi) x y clockwise j (retB, retH, used, image)
  | j == deg + 1 = (True, retH, used, image)
  | otherwise    =
    let used2  = replicate cartvert False;
        image2 = replicate cartvert (-1);
        image3 = image2 & ix 0 .~ clockwise;
        qZ0 = head qZ;
        qZ1 = qZ !! 1;
        image4 = image3 & ix qZ0 .~ x;
        image5 = image4 & ix qZ1 .~ y;
        used3 = used2 & ix x .~ True;
        used4 = used3 & ix y .~ True;

        f n@(retB, _, _, _) x cont
          | not retB  = n
          | otherwise = cont $ rootedSubConfSub adjmat degree question clockwise n x;
        retPack@(retB', retH', used', image') = myLoop f id (False, retH, used4, image5) [2, 3 .. 1024]
    in if not retB
      then
        retPack
      else
        -- test if image is well-positioned
        let cc    = if j == 1 then 2 * deg else deg + j - 1;
            usedJ = used' !! j;
            usedD = used' !! (deg + j);
            usedC = used' !! cc
        in if not usedJ && usedD && usedC
          then
            retPack
          else
            rootedSubConf degree deg adjmat question x y clockwise (j+1) retPack


rootedSubConfSub :: TpAdjmat -> TpVertices -> TpQuestion -> Int -> TpConfPack -> Int -> TpConfPack
rootedSubConfSub adjmat degree question@(qU, qV, qZ, qXi) clockwise (_, retH, used, image) q =
  let qUQ      = qU !! q;
      qVQ      = qV !! q;
      qZQ      = qZ !! q;
      qXiQ     = qXi !! q;
      imageQUQ = image !! qUQ;
      imageQVQ = image !! qVQ;
      w        = if clockwise == 0
                  then (adjmat ^?! ix imageQVQ) ^?! ix imageQUQ
                  else (adjmat ^?! ix imageQUQ) ^?! ix imageQVQ
      degreeW  = degree !! w
      usedW    = used !! w
  in if (qUQ < 0) || (w == -1) || (qXiQ /= 0 && qXiQ /= degreeW) || usedW
    then (False, retH, used,                image              )
    else (True,  retH, used & ix w .~ True, image & ix qZQ .~ w)



