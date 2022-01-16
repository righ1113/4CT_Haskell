module ReLibStrip where

import CoLibCConst   ( edges, mverts, TpConfmat, TpEdgeno, TpGetENPack )
import Control.Arrow ( (<<<), ArrowChoice((|||)) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


-- Now we must list the edges between the interior and the ring
strip :: TpConfmat -> TpEdgeno
strip gConf = stripSub3 gConf ring done term1 edgeno2 0 where
  verts  = head gConf !! (0 + 1)
  ring   = gConf !! (0 + 1) !! 1 -- ring-size
  edgeno = stripSub1 1 ring (replicate edges $ replicate edges 0)
  done   = replicate mverts False
  term0  = 3 * (verts - 1) - ring
  -- 2. stripSub2
  -- This eventually lists all the internal edges of the configuration
  (edgeno2, term1)  = stripSub2 edgeno gConf verts ring done term0 (ring + 1) 1 (replicate mverts 0) 0 0


stripSub1 :: Int -> Int -> TpEdgeno -> TpEdgeno
stripSub1 v ring edgeno
  | v > ring  = edgeno
  | otherwise = stripSub1 (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v


stripSub2 :: TpEdgeno -> TpConfmat -> Int -> Int -> [Bool] -> Int -> Int -> Int -> [Int] -> Int -> Int -> (TpEdgeno, Int)
stripSub2 edgeno gConf verts ring done term i best max maxint maxes
  | i > verts = (edgeno, term) -- This eventually lists all the internal edges of the configuration
  | otherwise = stripSub2 edgeno2 gConf verts ring done2 term (i + 1) best2 max2 maxint2 maxes2 where
      (maxint2, maxes2, max2) = stripSub2Sub1 gConf verts ring done maxint maxes max (ring + 1)
      maxdeg                  = 0
      best2                   = stripSub2Sub2 gConf maxes2 max2 maxdeg 1 0
      d                       = (gConf !! (best + 2)) !! (0 + 1)
      previous                = done !! ((gConf !! (best + 2)) !! (d + 1))
      first                   = stripSub2Sub3 gConf done best2 previous 1 d
      (edgeno2, done2)        = stripSub2Sub4 edgeno gConf done term best2 d first first


-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
stripSub2Sub1 :: TpConfmat -> Int -> Int -> [Bool] -> Int -> Int -> [Int] -> Int -> (Int, Int, [Int])
stripSub2Sub1 gConf verts ring done maxint maxes max v
  | v > verts = (maxint, maxes, max)
  | done !! v = stripSub2Sub1 gConf verts ring done maxint  maxes  max  (v + 1)
  | otherwise = stripSub2Sub1 gConf verts ring done maxint2 maxes2 max2 (v + 1) where
      inter = inInterval (gConf !! (v + 2)) done
      (maxint2, maxes2, max2)
        = if inter > maxint then (inter, 1, max & ix 1 .~ v) else (maxint, maxes + 1, max & ix (maxes + 1) .~ v)


-- From the terms in max we choose the one of maximum degree
-- So now, the vertex "best" will be the next vertex to be done
stripSub2Sub2 :: TpConfmat -> Int -> [Int] -> Int -> Int -> Int -> Int
stripSub2Sub2 gConf maxes max maxdeg h best
  | h > maxes = best
  | otherwise = stripSub2Sub2 gConf maxes max maxdeg2 (h + 1) best2 where
      d                = (gConf !! (max !! h + 2)) !! (0 + 1)
      (maxdeg2, best2) = if d > maxdeg then (d, max !! h) else (maxdeg, best)


stripSub2Sub3 :: TpConfmat -> [Bool] -> Int -> Bool -> Int -> Int -> Int
stripSub2Sub3 gConf done best previous first d
  | not previous && doneGConf = first
  | first > d = 1
  | otherwise = stripSub2Sub3 gConf done best doneGConf (first + 1) d where
      doneGConf = done !! ((gConf !! (best + 2)) !! (first + 1))


stripSub2Sub4 :: TpEdgeno -> TpConfmat -> [Bool] -> Int -> Int -> Int -> Int -> Int -> (TpEdgeno, [Bool])
stripSub2Sub4 edgeno gConf done term best d first h
  | not $ done !! gConfBH = (edgeno , done)
  | h == d && first == 1  = (edgeno3, done)
  | h == d && first /= 1  = stripSub2Sub4 edgeno3 gConf done2 (term - 1) best d first 0
  | otherwise             = stripSub2Sub4 edgeno3 gConf done2 (term - 1) best d first (h + 1) where
      gConfBH = (gConf !! (best + 2)) !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gConfBH) .~ term
      edgeno3 = edgeno2 & (ix gConfBH <<< ix best) .~ term
      done2   = done & ix best .~ True


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> Int -> TpEdgeno
stripSub3 gConf ring done term edgeno j
  | j > ring  = edgeno
  | done !! u = stripSub3 gConf ring done2 term2 edgeno2 (j + 1)
  | otherwise = stripSub3 gConf ring done3 term3 edgeno3 (j + 1) where
      best = stripSub3Sub1 gConf ring done 0 0 1
      grav = gConf !! (best + 2)
      u    = if best > 1 then best - 1 else ring
      (done2, term2, edgeno2) = stripSub3Sub2 grav done term edgeno best (grav !! (0 + 1) - 1) True
      (done3, term3, edgeno3) = stripSub3Sub2 grav done term edgeno best 2                     False


stripSub3Sub1 :: TpConfmat -> Int -> [Bool] -> Int -> Int -> Int -> Int
stripSub3Sub1 gConf ring done maxint best v
  | v > ring                                        = best
  | done !! v || not (done !! v) && inter <= maxint = stripSub3Sub1 gConf ring done maxint best (v + 1)
  | otherwise                                       = stripSub3Sub1 gConf ring done inter  v    (v + 1) where
      u        = if v > 1     then v - 1 else ring
      w        = if v < ring  then v + 1 else 1
      doneIntU = if done !! u then 1     else 0
      doneIntW = if done !! w then 1     else 0
      inter    = 3 * (gConf !! (v + 2)) !! (0 + 1) + 4 * (doneIntU + doneIntW)


stripSub3Sub2 :: [Int] -> [Bool] -> Int -> TpEdgeno -> Int -> Int -> Bool -> ([Bool], Int, TpEdgeno)
stripSub3Sub2 grav done term edgeno best h flg
  | flg     && h <= 2                    = (done, term, edgeno)
  | flg     && h >  2                    = stripSub3Sub2 grav done2 (term - 1) edgeno3 best (h - 1) True
  | not flg && h > (grav !! (0 + 1) - 1) = (done, term, edgeno)
  | otherwise = stripSub3Sub2 grav done2 (term + 1) edgeno3 best (h - 1) False where
      edgeno2 = edgeno  & (ix best <<< ix gravH1) .~ term
      edgeno3 = edgeno2 & (ix gravH1 <<< ix best) .~ term
      done2   = done & ix best .~ True
      gravH1  = grav !! (h + 1)


-- ##############################################
strip2 :: TpConfmat -> TpEdgeno
strip2 gConf = (getEdgenoSub3 0 . getEdgenoSub2 (ring + 1) 1 (replicate mverts 0) . getEdgenoSub1) gConf where
  ring = gConf !! 1 !! 1


getEdgenoSub1 :: TpConfmat -> TpGetENPack
getEdgenoSub1 gConf = (gConf, verts, ring, done, term, edgeno) where
  verts  = head (gConf !! 1)
  ring   = gConf !! 1 !! 1
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


-- This eventually lists all the internal edges of the configuration
getEdgenoSub2 :: Int -> Int -> [Int] -> TpGetENPack -> TpGetENPack
getEdgenoSub2 i best max pack@(gConf, verts, ring, done, term, edgeno) 
  | i > verts = pack -- This eventually lists all the internal edges of the configuration
  | otherwise = getEdgenoSub2 (i + 1) best2 max2 pack2 where
      d                    = (gConf !! (best + 2)) !! 1
      (best2, max2, pack2) = (getES2Sub4 (-1) d . getES2Sub3 1 d . getES2Sub2 0 1 best . getES2Sub1 0 0 max (ring + 1)) pack


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
getES2Sub2 maxdeg h best big@(maxes, max, pack) = big


getES2Sub3 :: Int -> Int -> (Int, [Int], TpGetENPack) -> (Int, Int, [Int], TpGetENPack)
getES2Sub3 first d (best, max, pack@(gConf, verts, _ring, done, _term, _edgeno)) = (first, best, max, pack) where
  previous = done !! ((gConf !! (best + 2)) !! (d + 1))


getES2Sub4 :: Int -> Int -> (Int, Int, [Int], TpGetENPack) -> (Int, [Int], TpGetENPack)
getES2Sub4 h d (first, best, max, pack) = (best, max, pack)


-- Now we must list the edges between the interior and the ring
getEdgenoSub3 :: Int -> TpGetENPack -> TpEdgeno
getEdgenoSub3 i pack@(gConf, verts, ring, done, term, edgeno)
  | True = edgeno --i > ring  = edgeno
  -- | done !! u = getEdgenoSub3 (i + 1) (gConf, verts, ring, done2, term2, edgeno2)
  | otherwise = getEdgenoSub3 (i + 1) pack2 where
      best = stripSub3Sub1 gConf ring done 0 0 1
      grav = gConf !! (best + 2)
      u    = if best > 1 then best - 1 else ring
      (done2, term2, edgeno2) = stripSub3Sub2 grav done term edgeno best (grav !! (0 + 1) - 1) True
      (done3, term3, edgeno3) = stripSub3Sub2 grav done term edgeno best 2                     False
      pack2 = (getES3Sub2 True ||| getES3Sub2 False) <<< getES3Sub1 0 0 1 $ pack


getES3Sub1 :: Int -> Int -> Int -> TpGetENPack -> Either (Int, Int, TpGetENPack) (Int, Int, TpGetENPack) 
getES3Sub1 = undefined
getES3Sub2 :: Bool -> (Int, Int, TpGetENPack) -> TpGetENPack
getES3Sub2 flg (best, h, pack@(gConf, verts, ring, done, term, edgeno))
  | flg     && h <= 2        = pack
  | flg     && h >  2        = getES3Sub2 flg (best, h - 1, (gConf, verts, ring, done2, term - 1, edgeno3))
  | not flg && h > head grav = pack
  | otherwise                = getES3Sub2 flg (best, h + 1, (gConf, verts, ring, done2, term - 1, edgeno3)) where
      grav    = gConf !! (best + 2)
      done2   = done & ix best .~ True
      gravH1  = grav !! (h + 1)
      edgeno2 = edgeno  & (ix best <<< ix gravH1) .~ term
      edgeno3 = edgeno2 & (ix gravH1 <<< ix best) .~ term


-- ##############################################
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



