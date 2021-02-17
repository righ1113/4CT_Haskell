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
  term1  = stripSub2 gConf verts ring done term0


stripSub1 :: Int -> Int -> TpEdgeno -> TpEdgeno
stripSub1 v ring edgeno
  | v > ring  = edgeno
  | otherwise = stripSub1 (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v


stripSub2 :: TpConfmat -> Int -> Int -> [Bool] -> Int -> Int
stripSub2 gConf verts ring done term0 = term0


stripSub3 :: TpConfmat -> Int -> [Bool] -> Int -> TpEdgeno -> TpEdgeno
stripSub3 gConf ring done term1 edgeno = edgeno



