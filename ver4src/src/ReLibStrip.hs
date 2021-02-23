module ReLibStrip where

import CoLibCConst   ( edges, mverts, TpConfmat, TpEdgeno )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )


-- 3. stripSub3
-- Now we must list the edges between the interior and the ring
strip :: TpConfmat -> TpEdgeno
strip gConf = stripSub3 gConf ring done term1 edgeno where
  verts  = head gConf !! (0 + 1)
  ring   = gConf !! (0 + 1) !! 1 -- ring-size
  edgeno = stripSub1 1 ring (replicate edges $ replicate edges 0)
  done   = replicate mverts False
  term0  = 3 * (verts - 1) - ring
  -- 2. stripSub2
  -- This eventually lists all the internal edges of the configuration
  term1  = stripSub2 gConf verts ring done term0 (ring + 1) 1 (replicate mverts 0) 0 0


stripSub1 :: Int -> Int -> TpEdgeno -> TpEdgeno
stripSub1 v ring edgeno
  | v > ring  = edgeno
  | otherwise = stripSub1 (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v


stripSub2 :: TpConfmat -> Int -> Int -> [Bool] -> Int -> Int -> Int -> [Int] -> Int -> Int -> Int
stripSub2 gConf verts ring done term0 i best max maxint maxes
  | i > verts = term0 -- This eventually lists all the internal edges of the configuration
  | otherwise = stripSub2 gConf verts ring done term0 (i + 1) best2 max2 maxint2 maxes2 where
      (maxint2, maxes2, max2) = stripSub2Sub1 gConf verts ring maxint maxes max
      maxdeg = 0
      best2 = stripSub2Sub2 gConf maxes2 max2 maxdeg 1
      d        = (gConf !! (best + 2)) !! (0 + 1)
      first    = 1
      previous = done !! ((gConf !! (best + 2)) !! (d + 1))
      {-
      while previous || !done[g_conf[best + 2][first + 1]]
        previous = done[g_conf[best + 2][1 + first]]
        first += 1
        (first = 1; break) if first > d
      end

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


stripSub2Sub1 :: TpConfmat -> Int -> Int -> Int -> Int -> [Int] -> (Int, Int, [Int])
stripSub2Sub1 gConf verts ring maxint maxes max = (maxint, maxes, max)
{-
      # First we find all vertices from the interior that meet the "done"
      # vertices in an interval, and write them in max[1] .. max[maxes]
      ((ring + 1)..verts).each do |v|
        next if done[v]
        inter = in_interval g_conf[v + 2], done
        if inter > maxint
          maxint = inter
          maxes  = 1
          max[1] = v
        elsif inter == maxint
          maxes += 1
          max[maxes] = v
        end
      end
-}

stripSub2Sub2 :: TpConfmat -> Int -> [Int] -> Int -> Int -> Int
stripSub2Sub2 gConf maxes2 max2 maxdeg h = 2
{-
# From the terms in max we choose the one of maximum degree
maxdeg = 0
(1..maxes).each do |h|
  d = g_conf[max[h] + 2][0 + 1]
  if d > maxdeg
    maxdeg = d
    best   = max[h]
  end
end
# So now, the vertex "best" will be the next vertex to be done
-}


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> TpEdgeno
stripSub3 gConf ring done term1 edgeno = edgeno



