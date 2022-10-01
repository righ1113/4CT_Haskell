module ReLibStrip( getEdgeNo, strip ) where

import CoLibCConst   ( edges, debugLogStrip, mverts, TpConfmat, TpEdgeNo, TpGetENPack )
import Control.Arrow ( (<<<), (|||) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.List     ( sortOn )
import Data.Ord      ( Down(..) )


getEdgeNo :: Int -> Int -> TpConfmat -> TpEdgeNo
getEdgeNo vertex ring gConf = debugLogStrip ("$$$ vertex: " ++ show edgeNo) edgeNo where
  edgeNo   = getEdgeNoSub edgeList [11,13,12,14,15,16,17,23,21,22,19,20,18,24,25,26,28,27,35,34,30,29,36,37,38,31,32,33,-1] edgeNo0
  edgeNo0  = newEdgeNo 1 ring (replicate edges $ replicate edges 0)
  edgeList = concatMap (toTupleList []) $ getEdgeList vertex ring gConf


getEdgeNoSub :: [[Int]] -> [Int] -> TpEdgeNo -> TpEdgeNo
getEdgeNoSub []            _               edgeNo = edgeNo
getEdgeNoSub ([a, b] : xs) ts@(term : tss) edgeNo = edgeNo4 where
  edgeNo4
    | edgeNo !! a !! b == 0 = debugLogStrip ("[a, b] set "    ++ show a ++ " " ++ show b ++ " " ++ show term) $ getEdgeNoSub xs tss edgeNo3
    | otherwise             = debugLogStrip ("[a, b] cancel " ++ show a ++ " " ++ show b ++ " " ++ show term) $ getEdgeNoSub xs ts  edgeNo
  edgeNo2 = edgeNo  & (ix a <<< ix b) .~ term
  edgeNo3 = edgeNo2 & (ix b <<< ix a) .~ term
getEdgeNoSub _ _ _                                = error "getEdgeNoSub error!!"


newEdgeNo :: Int -> Int -> TpEdgeNo -> TpEdgeNo
newEdgeNo v ring edgeNo
  | v > ring  = edgeNo
  | otherwise = newEdgeNo (v + 1) ring edgeNo2 where
      u       = if v > 1 then v - 1 else ring
      edgeNo1 = edgeNo  & (ix u <<< ix v) .~ v
      edgeNo2 = edgeNo1 & (ix v <<< ix u) .~ v


getEdgeList :: Int -> Int -> TpConfmat -> [[Int]]
getEdgeList vertex ring gConf = debugLogStrip ("$$$ edgeLists: " ++ show (concatMap (toTupleList []) (edgeList1 ++ edgeList2))) (reverse edgeList1 ++ reverse edgeList2) where
  edgeLists = splitAt ring . take vertex . drop 3 $ gConf
  edgeList1 = zipWith (++) (map (: []) [1..ring])          (map (sortOn Down . filter (> ring))        $ fst edgeLists)
  edgeList2 = zipWith (++) (map (: []) [(ring+1)..vertex]) (map (sortOn Down . filter (> ring) . tail) $ snd edgeLists)


toTupleList :: [[Int]] -> [Int] -> [[Int]]
toTupleList _   []       = []
toTupleList _   [_]      = []
toTupleList acm [x, y]   = acm ++ [[x,y]] 
toTupleList acm (x:y:xs) = toTupleList (acm ++ [[x,y]]) (x:xs)




strip :: Int -> TpConfmat -> TpEdgeNo
strip ring = getEdgenoSub3 0 . getEdgenoSub2 (ring + 1) 1 (replicate mverts 0) . getEdgenoSub1 ring


-- ======== getEdgenoSub1 ========
getEdgenoSub1 :: Int -> TpConfmat -> TpGetENPack
getEdgenoSub1 ring gConf = (gConf, verts, ring, done, term, edgeno) where
  verts  = head (gConf !! 1)
  done   = replicate mverts False
  term   = 3 * (verts - 1) - ring
  edgeno = newEdgeno 1 ring (replicate edges $ replicate edges 0)


newEdgeno :: Int -> Int -> TpEdgeNo -> TpEdgeNo
newEdgeno v ring edgeno
  | v > ring  = edgeno
  | otherwise = newEdgeno (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v


-- ======== getEdgenoSub2 ========
-- This eventually lists all the internal edges of the configuration
getEdgenoSub2 :: Int -> Int -> [Int] -> TpGetENPack -> TpGetENPack
getEdgenoSub2 i best max0 pack@(gConf, verts, ring, done, _, _) 
  | i > verts = pack -- This eventually lists all the internal edges of the configuration
  | otherwise = getEdgenoSub2 (i + 1) best2 max2 pack2 where
      d                    =          gConf !! (best + 2) !! 1
      previous             = done !! (gConf !! (best + 2) !! (d + 1))
      (best2, max2, pack2) = (getES2DoneBestTrue . getES2Sub4 (-1) d . getES2Sub3 previous 1 d . getES2Sub2 0 1 best . getES2Sub1 0 0 max0 (ring + 1)) pack


-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
getES2Sub1 :: Int -> Int -> [Int] -> Int -> TpGetENPack -> (Int, [Int], TpGetENPack)
getES2Sub1 maxint maxes max0 v pack@(gConf, verts, _, done, _, _)
  | v > verts = (maxes, max0, pack)
  | done !! v = getES2Sub1 maxint  maxes  max0 (v + 1) pack
  | otherwise = getES2Sub1 maxint2 maxes2 max2 (v + 1) pack where
      inter = inInterval (gConf !! (v + 2)) done
      (maxint2, maxes2, max2)
            = if inter > maxint then (inter, 1, max0 & ix 1 .~ v) else (maxint, maxes + 1, max0 & ix (maxes + 1) .~ v)


-- From the terms in max we choose the one of maximum degree
-- So now, the vertex "best" will be the next vertex to be done
getES2Sub2 :: Int -> Int -> Int -> (Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2Sub2 maxdeg h best big@(maxes, max0, pack@(gConf, _, _, _, _, _))
  | h > maxes = (best, max0, pack)
  | otherwise = getES2Sub2 maxdeg2 (h + 1) best2 big where
      d                = gConf !! (max0 !! h + 2) !! 1
      (maxdeg2, best2) = if d > maxdeg then (d, max0 !! h) else (maxdeg, best)


getES2Sub3 :: Bool -> Int -> Int -> (Int, [Int], TpGetENPack) -> (Int, Int, [Int], TpGetENPack)
getES2Sub3 previous first d (best, max0, pack@(gConf, _, _, done, _, _))
  | not previous && doneGConf = (first, best, max0, pack)
  | first > d                 = (1,     best, max0, pack)
  | otherwise                 = getES2Sub3 doneGConf (first + 1) d (best, max0, pack) where
      doneGConf = done !! (gConf !! (best + 2) !! (first + 1))


getES2Sub4 :: Int -> Int -> (Int, Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2Sub4 (-1) d (first, best, max0, pack) = getES2Sub4 first d (first, best, max0, pack)
getES2Sub4 h    d (first, best, max0, pack@(gConf, verts, ring, done, term, edgeno))
  | not $ done !! gConfBH = debugLogStrip ("BH: " ++ show gConfBH ++ " " ++ show best ++ " " ++ show h ++ " " ++ show done) (best, max0, pack)
  | h == d && first == 1 = (best, max0, (gConf, verts, ring, done, term - 1, edgeno3))
  | h == d && first /= 1  = getES2Sub4 1       d (first, best, max0, (gConf, verts, ring, debugLogStrip "none: " done, term - 1, edgeno3))
  | otherwise             = getES2Sub4 (h + 1) d (first, best, max0, (gConf, verts, ring, debugLogStrip ("done: " ++ show done) done, term - 1, edgeno3)) where
      gConfBH = gConf !! (best + 2) !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gConfBH) .~ term
      edgeno3 = edgeno2 & (ix gConfBH <<< ix best) .~ term


getES2DoneBestTrue :: (Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2DoneBestTrue (best, max0, (gConf, verts, ring, done, term, edgeno)) = (best, max0, (gConf, verts, ring, done2, term, edgeno)) where
  done2 = done & ix best .~ True


-- ======== getEdgenoSub3 ========
-- Now we must list the edges between the interior and the ring
getEdgenoSub3 :: Int -> TpGetENPack -> TpEdgeNo
getEdgenoSub3 i (gConf, verts, ring, done, term, edgeno)
  | i >= ring = edgeno
  | otherwise = debugLogStrip ("term: " ++ show term) $ getEdgenoSub3 (i + 1) pack2 where
      pack2 = getES3DoneBestTrue <<< (getES3Sub2 True ||| getES3Sub2 False) <<< getES3Sub1 0 0 1 $ (gConf, verts, ring, done, term, edgeno)


getES3Sub1 :: Int -> Int -> Int -> TpGetENPack -> Either (Int, Int, TpGetENPack) (Int, Int, TpGetENPack) 
getES3Sub1 maxint best v pack@(gConf, _, ring, done, _, _)
  | v > ring                                          = debugLogStrip ("best: " ++ show best ++ " " ++ show done) ret
  | done !! v || (not (done !! v) && inter <= maxint) = getES3Sub1 maxint best (v + 1) pack
  | otherwise                                         = getES3Sub1 inter  v    (v + 1) pack where
      u        = if v > 1     then v - 1 else ring
      w        = if v < ring  then v + 1 else 1
      doneIntU = if done !! u then 1     else 0
      doneIntW = if done !! w then 1     else 0
      inter    = 3 * gConf !! (v + 2) !! 1 + 4 * (doneIntU + doneIntW)
      u2       = if best > 1 then best - 1 else ring
      ret      = if done !! u2 then Left (best, gConf !! (best + 2)!! 1 - 1, pack) else Right (best, 2, pack)


getES3Sub2 :: Bool -> (Int, Int, TpGetENPack) -> (Int, TpGetENPack)
getES3Sub2 flg (best, h, pack@(gConf, verts, ring, done, term, edgeno))
  | flg     && h <  2             = (best, pack)
  | flg     && h >= 2             = getES3Sub2 flg (best, h - 1, (gConf, verts, ring, done, term - 1, edgeno3))
  | not flg && h >  grav !! 1 - 1 = (best, pack)
  | otherwise                     = getES3Sub2 flg (best, h + 1, (gConf, verts, ring, done, term - 1, edgeno3)) where
      grav    = debugLogStrip ("gConf: " ++ show gConf ++ " " ++ show h) gConf !! (best + 2)
      gravH1  = grav !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gravH1) .~ term
      edgeno3 = edgeno2 & (ix gravH1 <<< ix best) .~ term


getES3DoneBestTrue :: (Int, TpGetENPack) -> TpGetENPack
getES3DoneBestTrue (best, (gConf, verts, ring, done, term, edgeno)) = (gConf, verts, ring, done2, term, edgeno) where
  done2 = done & ix best .~ True


-- ======== inInterval ========
inInterval :: [Int] -> [Bool] -> Int
inInterval grav done
  | first == d  = if done !! (grav !! (d + 1)) then 1 else 0
  | last0  == d = length0
  | first > 1   = inIntervalSub1 grav done d length0 (last0 + 2)
  | chg         = 0
  | otherwise   = len where
      d          = grav !! (0 + 1)
      first      = getFirst grav done d 1
      last0      = getLast  grav done d first
      length0    = last0 - first + 1
      (len, chg) = inIntervalSub2 grav done d False length0 (last0 + 2)


getFirst :: [Int] -> [Bool] -> Int -> Int -> Int
getFirst grav done d first
  | first >= d || done !! (grav !! (first + 1)) = first
  | otherwise = getFirst grav done d (first + 1)


getLast :: [Int] -> [Bool] -> Int -> Int -> Int
getLast grav done d last0
  | last0 >= d || not (done !! (grav !! (1 + last0 + 1))) = last0
  | otherwise = getFirst grav done d (last0 + 1)


inIntervalSub1 :: [Int] -> [Bool] -> Int -> Int -> Int -> Int
inIntervalSub1 grav done d length0 j
  | j > d                     = length0
  | done !! (grav !! (j + 1)) = 0
  | otherwise                 = inIntervalSub1 grav done d length0 (j + 1)


inIntervalSub2 :: [Int] -> [Bool] -> Int -> Bool -> Int -> Int -> (Int, Bool)
inIntervalSub2 grav done d worried length0 j
  | j > d                                      = (length0, False)
  | done !! (grav !! (j + 1))                  = inIntervalSub2 grav done d True    (length0 + 1) (j + 1)
  | not (done !! (grav !! (j + 1))) && worried = (length0, True)
  | otherwise                                  = inIntervalSub2 grav done d worried length0       (j + 1)



