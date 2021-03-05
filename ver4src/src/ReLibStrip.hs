module ReLibStrip where

import CoLibCConst   ( edges, mverts, TpConfmat, TpEdgeno )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


-- Now we must list the edges between the interior and the ring
strip :: TpConfmat -> TpEdgeno
strip gConf = stripSub3 gConf ring done term1 edgeno2 where
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
      first                   = stripSub2Sub3 gConf done best2 previous 1
      (edgeno2, done2)        = stripSub2Sub4 edgeno gConf done term best2 d first


-- First we find all vertices from the interior that meet the "done"
-- vertices in an interval, and write them in max[1] .. max[maxes]
stripSub2Sub1 :: TpConfmat -> Int -> Int -> [Bool] -> Int -> Int -> [Int] -> Int -> (Int, Int, [Int])
stripSub2Sub1 gConf verts ring done maxint maxes max v
  | v > verts = (maxint, maxes, max)
  | done !! v = stripSub2Sub1 gConf verts ring done maxint  maxes  max  (v + 1)
  | otherwise = stripSub2Sub1 gConf verts ring done maxint2 maxes2 max2 (v + 1) where
      inter = 0 --in_interval g_conf[v + 2], done
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


stripSub2Sub3 :: TpConfmat -> [Bool] -> Int -> Bool -> Int -> Int
stripSub2Sub3 gConf done best previous first = first
{-
  while previous || !done[g_conf[best + 2][first + 1]]
    previous = done[g_conf[best + 2][1 + first]]
    first += 1
    (first = 1; break) if first > d
  end
-}


stripSub2Sub4 :: TpEdgeno -> TpConfmat -> [Bool] -> Int -> Int -> Int -> Int -> (TpEdgeno, [Bool])
stripSub2Sub4 edgeno gConf done term best d first = (edgeno, done)
{-
  h = first
  while done[g_conf[best + 2][h + 1]]
    @edgeno[best][g_conf[best + 2][h + 1]] = term
    @edgeno[g_conf[best + 2][h + 1]][best] = term
    term -= 1
    if h == d
      break if first == 1
      h = 0
    end
    h += 1
  end
  done[best] = true
-}


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> TpEdgeno
stripSub3 gConf ring done term1 edgeno = edgeno



