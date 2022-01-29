module ReLibStrip where

import CoLibCConst   ( edges, mverts, TpConfmat, TpEdgeno, TpGetENPack )
import Control.Arrow ( (<<<), ArrowChoice((|||)) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Debug.Trace   ( trace )


strip :: Int -> TpConfmat -> TpEdgeno
strip ring = getEdgenoSub3 0 . getEdgenoSub2 (ring + 1) 1 (replicate mverts 0) . getEdgenoSub1 ring


-- ======== getEdgenoSub1 ========
getEdgenoSub1 :: Int -> TpConfmat -> TpGetENPack
getEdgenoSub1 ring gConf = (gConf, verts, ring, done, term, edgeno) where
  verts  = head (gConf !! 1)
  done   = replicate mverts False
  term   = 3 * (verts - 1) - ring
  edgeno = newEdgeno 1 ring (replicate edges $ replicate edges 0)


newEdgeno :: Int -> Int -> TpEdgeno -> TpEdgeno
newEdgeno v ring edgeno
  | v > ring  = edgeno
  | otherwise = newEdgeno (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v


-- ======== getEdgenoSub2 ========
-- This eventually lists all the internal edges of the configuration
getEdgenoSub2 :: Int -> Int -> [Int] -> TpGetENPack -> TpGetENPack
getEdgenoSub2 i best max pack@(gConf, verts, ring, done, term, edgeno) 
  | i > verts = pack -- This eventually lists all the internal edges of the configuration
  | otherwise = getEdgenoSub2 (i + 1) best2 max2 pack2 where
      d                    = gConf !! (best + 2) !! 1
      (best2, max2, pack2) = (getES2DoneBestTrue . getES2Sub4 (-1) d . getES2Sub3 1 d . getES2Sub2 0 1 best . getES2Sub1 0 0 max (ring + 1)) pack


-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
getES2Sub1 :: Int -> Int -> [Int] -> Int -> TpGetENPack -> (Int, [Int], TpGetENPack)
getES2Sub1 maxint maxes max v pack@(gConf, verts, _, done, _, _)
  | v > verts = (maxes, max, pack)
  | done !! v = getES2Sub1 maxint  maxes  max  (v + 1) pack
  | otherwise = getES2Sub1 maxint2 maxes2 max2 (v + 1) pack where
      inter = inInterval (gConf !! (v + 2)) done
      (maxint2, maxes2, max2)
            = if inter > maxint then (inter, 1, max & ix 1 .~ v) else (maxint, maxes + 1, max & ix (maxes + 1) .~ v)


-- From the terms in max we choose the one of maximum degree
-- So now, the vertex "best" will be the next vertex to be done
getES2Sub2 :: Int -> Int -> Int -> (Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2Sub2 maxdeg h best big@(maxes, max, pack@(gConf, _, _, _, _, _))
  | h > maxes = (best, max, pack)
  | otherwise = getES2Sub2 maxdeg2 (h + 1) best2 big where
      d                = gConf !! (max !! h + 2) !! 1
      (maxdeg2, best2) = if d > maxdeg then (d, max !! h) else (maxdeg, best)


getES2Sub3 :: Int -> Int -> (Int, [Int], TpGetENPack) -> (Int, Int, [Int], TpGetENPack)
getES2Sub3 first d (best, max, pack@(gConf, _, _, done, _, _))
  | first > d                 = (1,     best, max, pack)
  | not previous && doneGConf = (first, best, max, pack)
  | otherwise                 = getES2Sub3 (first + 1) d (best, max, pack) where
      previous  = done !! ((gConf !! (best + 2)) !! (d     + 1))
      doneGConf = done !! ((gConf !! (best + 2)) !! (first + 1))


getES2Sub4 :: Int -> Int -> (Int, Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2Sub4 (-1) d (first, best, max, pack) = getES2Sub4 first d (first, best, max, pack)
getES2Sub4 h    d (first, best, max, pack@(gConf, verts, ring, done, term, edgeno))
  | not $ done !! gConfBH = trace ("BH: " ++ show gConfBH ++ " " ++ show best ++ " " ++ show h ++ " " ++ show done) (best, max, pack) --not $ done !! gConfBH
  | h == d && first == 1 = (best, max, (gConf, verts, ring, done, term, edgeno3))
  | h == d && first /= 1  = getES2Sub4 0       d (first, best, max, (gConf, verts, ring, trace "none: " done, term - 1, edgeno3))
  | otherwise             = getES2Sub4 (h + 1) d (first, best, max, (gConf, verts, ring, trace ("done: " ++ show done) done, term - 1, edgeno3)) where
      term2 = error "50"
      gConfBH = gConf !! (best + 2) !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gConfBH) .~ term
      edgeno3 = edgeno2 & (ix gConfBH <<< ix best) .~ term


getES2DoneBestTrue :: (Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2DoneBestTrue (best, max, (gConf, verts, ring, done, term, edgeno)) = (best, max, (gConf, verts, ring, done2, term, edgeno)) where
  done2 = done & ix best .~ True


-- ======== getEdgenoSub3 ========
-- Now we must list the edges between the interior and the ring
getEdgenoSub3 :: Int -> TpGetENPack -> TpEdgeno
getEdgenoSub3 i pack@(gConf, verts, ring, done, term, edgeno)
  | i >= ring = edgeno
  | otherwise = trace ("term: " ++ show term) $ getEdgenoSub3 (i + 1) pack2 where
      pack2 = getES3DoneBestTrue <<< (getES3Sub2 True ||| getES3Sub2 False) <<< getES3Sub1 0 0 1 $ (gConf, verts, ring, done, term, edgeno)


getES3Sub1 :: Int -> Int -> Int -> TpGetENPack -> Either (Int, Int, TpGetENPack) (Int, Int, TpGetENPack) 
getES3Sub1 maxint best v pack@(gConf, verts, ring, done, term, edgeno)
  | v > ring                                          = trace ("best: " ++ show best ++ " " ++ show done) ret
  | done !! v || (not (done !! v) && inter <= maxint) = getES3Sub1 maxint best (v + 1) pack
  | otherwise                                         = getES3Sub1 inter  v    (v + 1) pack where
      u        = if v > 1     then v - 1 else ring
      w        = if v < ring  then v + 1 else 1
      doneIntU = if done !! u then 1     else 0
      doneIntW = if done !! w then 1     else 0
      inter    = 3 * gConf !! (v + 2) !! 1 + 4 * (doneIntU + doneIntW)
      u2       = if best > 1 then best - 1 else ring
      ret      = if done !! u2 then Left (best, gConf !! (best + 2)!! 1, pack) else Right (best, 2, pack)


getES3Sub2 :: Bool -> (Int, Int, TpGetENPack) -> (Int, TpGetENPack)
getES3Sub2 flg (best, h, pack@(gConf, verts, ring, done, term, edgeno))
  | flg     && h <= 2        = (best, pack)
  | flg     && h >  2        = getES3Sub2 flg (best, h - 1, (gConf, verts, ring, done, term - 1, edgeno3))
  | not flg && h > grav !! 1 = (best, pack)
  | otherwise                = getES3Sub2 flg (best, h + 1, (gConf, verts, ring, done, term - 1, edgeno3)) where
      grav    = trace ("gConf: " ++ show gConf ++ " " ++ show h) gConf !! (best + 2)
      gravH1  = grav !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gravH1) .~ term
      edgeno3 = edgeno2 & (ix gravH1 <<< ix best) .~ term


getES3DoneBestTrue :: (Int, TpGetENPack) -> TpGetENPack
getES3DoneBestTrue (best, (gConf, verts, ring, done, term, edgeno)) = (gConf, verts, ring, done2, term, edgeno) where
  done2 = done & ix best .~ True


-- ======== inInterval ========
inInterval :: [Int] -> [Bool] -> Int
inInterval grav done
  | first == d = if done !! (grav !! (d + 1)) then 1 else 0
  | last  == d = length
  | first > 1  = inIntervalSub1 grav done d length (last + 2)
  | chg        = 0
  | otherwise  = len where
      d          = grav !! (0 + 1)
      first      = getFirst grav done d 1
      last       = getLast  grav done d first
      length     = last - first + 1
      (len, chg) = inIntervalSub2 grav done d False length (last + 2)


getFirst :: [Int] -> [Bool] -> Int -> Int -> Int
getFirst grav done d first
  | first >= d || done !! (grav !! (first + 1)) = first
  | otherwise = getFirst grav done d (first + 1)


getLast :: [Int] -> [Bool] -> Int -> Int -> Int
getLast grav done d last
  | last >= d || not (done !! (grav !! (1 + last + 1))) = last
  | otherwise = getFirst grav done d (last + 1)


inIntervalSub1 :: [Int] -> [Bool] -> Int -> Int -> Int -> Int
inIntervalSub1 grav done d length j
  | j > d                     = length
  | done !! (grav !! (j + 1)) = 0
  | otherwise                 = inIntervalSub1 grav done d length (j + 1)


inIntervalSub2 :: [Int] -> [Bool] -> Int -> Bool -> Int -> Int -> (Int, Bool)
inIntervalSub2 grav done d worried length j
  | j > d                                      = (length, False)
  | done !! (grav !! (j + 1))                  = inIntervalSub2 grav done d True    (length + 1) (j + 1)
  | not (done !! (grav !! (j + 1))) && worried = (length, True)
  | otherwise                                  = inIntervalSub2 grav done d worried length       (j + 1)



