module ReLibStrip where

import CoLibCConst


-- 3. stripSub3
-- Now we must list the edges between the interior and the ring
strip :: TpConfmat -> TpEdgeno
strip gConf = stripSub3 gConf ring done term1 edgeno where
  verts  = head gConf !! (0 + 1)
  ring   = gConf !! (0 + 1) !! 1 -- ring-size
  edgeno = stripSub1 ring
  done   = replicate mverts False
  term0  = 3 * (verts - 1) - ring
  -- 2. stripSub2
  -- This eventually lists all the internal edges of the configuration
  term1  = stripSub2 gConf verts ring done term0


stripSub1 :: Int -> TpEdgeno
stripSub1 ring = replicate edges $ replicate edges 0
{-
  Array.new(Const::EDGES) { Array.new(Const::EDGES, 0)
  ring.times do |vv|
    v = vv + 1
    u = v > 1 ? v - 1 : ring
    @edgeno[u][v] = v
    @edgeno[v][u] = v
  end
-}


stripSub2 :: TpConfmat -> Int -> Int -> [Bool] -> Int -> Int
stripSub2 gConf verts ring done term0 = term0


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> TpEdgeno
stripSub3 gConf ring done term1 edgeno = edgeno



