module ReLibStrip where

import CoLibCConst   ( edges, mverts, TpConfmat, TpEdgeno )
import Control.Arrow ( (<<<) )
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


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> Int -> TpEdgeno
stripSub3 gConf ring done term1 edgeno j
  | j > ring  = edgeno
  | otherwise = stripSub3 gConf ring done term1 edgeno (j + 1)
{-
      ring.times do
        maxint = 0
        best   = 0
        v      = 1
        while v <= ring
          unless done[v]
            u          = v > 1 ? v - 1 : ring
            w          = v < ring ? v + 1 : 1
            done_int_u = done[u] ? 1 : 0
            done_int_w = done[w] ? 1 : 0
            inter      = 3 * g_conf[v + 2][0 + 1] + 4 * (done_int_u + done_int_w)
            if inter > maxint
              maxint = inter
              best = v
            end
          end
          v += 1
        end

        grav = g_conf[best + 2]
        u = best > 1 ? best - 1 : ring
        if done[u]
          (2..(grav[0 + 1] - 1)).reverse_each do |h|
            @edgeno[best][grav[h + 1]] = term
            @edgeno[grav[h + 1]][best] = term
            term -= 1
          end
        else
          (2..(grav[0 + 1] - 1)).each do |h|
            @edgeno[best][grav[h + 1]] = term
            @edgeno[grav[h + 1]][best] = term
            term -= 1
          end
        end
        done[best] = true
      end
-}



