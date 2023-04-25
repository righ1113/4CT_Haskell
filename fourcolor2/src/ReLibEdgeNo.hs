module ReLibEdgeNo( getEdgeNo ) where

import CoLibCConst   ( edgesM, debugLogStrip, mverts, TpConfFmt, TpEdgeNo, TpGetENPack )
import Control.Arrow ( (<<<), (|||) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.Function ( fix )



getEdgeNo :: Int -> Int -> Int -> TpConfFmt -> TpEdgeNo
getEdgeNo verts ring term = getEdgenoSub3 0 . getEdgenoSub2 (ring + 1) 1 (replicate mverts 0) . getEdgenoSub1 verts ring term

-- ======== getEdgenoSub1 ========
getEdgenoSub1 :: Int -> Int -> Int -> TpConfFmt -> TpGetENPack
getEdgenoSub1 verts ring term gConf = (gConf, verts, ring, done, term, edgeno) where
  done   = replicate mverts False
  edgeno = newEdgeno 1 ring (replicate edgesM $ replicate edgesM 0)

newEdgeno :: Int -> Int -> TpEdgeNo -> TpEdgeNo
newEdgeno v ring edgeno
  | v > ring  = edgeno
  | otherwise = newEdgeno (v + 1) ring edgeno2 where
      u       = if v > 1 then v - 1 else ring
      edgeno1 = edgeno  & (ix u <<< ix v) .~ v
      edgeno2 = edgeno1 & (ix v <<< ix u) .~ v

-- ======== getEdgenoSub2 ========
getEdgenoSub2 :: Int -> Int -> [Int] -> TpGetENPack -> TpGetENPack
getEdgenoSub2 cnt best max0 pack@(gConf, verts, ring, done, term, edgeNo)
  | cnt > verts = pack
  | otherwise   =
    -- First we find all vertices from the interior that meet the "done"
    -- vertices in an interval, and write them in max[1] .. max[maxes]
    let (_maxint2, maxes2, max2) = flip fix (0::Int, 0::Int, max0, ring + 1) $ \loop (i, e, m, v) -> case () of
          _ | v > verts -> (i, e, m)
            | done !! v  -> loop (i,     e,     m,  v + 1) -- continue
            | inter > i  -> loop (inter, 1,     m2, v + 1)
            | inter == i -> loop (i,     e + 1, m3, v + 1)
            | otherwise  -> loop (i,     e,     m,  v + 1) where
                inter = inInterval (gConf !! (v + 2)) done
                m2    = m & ix 1       .~ v
                m3    = m & ix (e + 1) .~ v
    -- From the terms in max we choose the one of maximum degree
        (_maxdeg2, best2) = flip fix (0::Int, best, 1) $ \loop (d', b, h) -> case () of
          _ | h > maxes2 -> (d', b)
            | dd > d'    -> loop (dd, b2, h + 1)
            | otherwise  -> loop (d', b,  h + 1) where
                dd = gConf !! (max2 !! h + 2) !! 1
                b2 = max2 !! h
    -- So now, the vertex "best" will be the next vertex to be done
        grav     = gConf !! (best2 + 2)
        d        = grav !! 1
        first    = 1
        previous = done !! (grav !! (d + 1))
        first2   = flip fix (first, previous, 0::Int) $ \loop (f, p, h) -> case () of
          _ | not p && (done !! (grav !! (f + 1))) -> f
            | f + 1 > d                            -> 1 -- break
            | otherwise                            -> loop (f + 1, p2, h + 1) where
                p2 = done !! (grav !! (f + 1))
    --
        (edgeNo2, term2) = debugLogStrip ("best2: " ++ show best2 ++ " " ++ show (grav !! (first2 + 1))) $ flip fix (edgeNo, term, first2) $ \loop (e, t, h) -> case () of
          _ | not $ done !! (grav !! (h + 1)) -> (e,  t)
            | h == d && first2 == 1           -> (e3, t - 1) -- break
            | h == d && first2 /= 1           -> loop (e3, t - 1, 1)
            | otherwise                       -> loop (e3, t - 1, h + 1) where
                gConfBH = grav !! (h + 1)
                e2      = debugLogStrip ("best2_2: " ++ show best2 ++ " " ++ show gConfBH ++ " " ++ show t) $ e  & (ix best2 <<< ix gConfBH) .~ t
                e3      = e2 & (ix gConfBH <<< ix best2) .~ t
    --
        done2 = done & ix best2 .~ True
    -- This eventually lists all the internal edges of the configuration
    in getEdgenoSub2 (cnt + 1) best2 max2 (gConf, verts, ring, done2, term2, edgeNo2)

-- ======== getEdgenoSub3 ========
-- Now we must list the edges between the interior and the ring
getEdgenoSub3 :: Int -> TpGetENPack -> TpEdgeNo
getEdgenoSub3 i pack@(_, _, ring, _, term, edgeno)
  | i >= ring = debugLogStrip ("$$$ vertex: " ++ show edgeno) edgeno
  | otherwise = debugLogStrip ("term: " ++ show term) $ getEdgenoSub3 (i + 1) pack2 where
      pack2 = getES3DoneBestTrue <<< (getES3Sub2 True ||| getES3Sub2 False) <<< getES3Sub1 0 0 1 $ pack

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
      ret      = if done !! u2 then Left (best, gConf !! (best + 2) !! 1 - 1, pack) else Right (best, 2, pack)

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
  | first == d = fromEnum $ done !! (grav !! (d + 1))
  | last0 == d = length0
  | first > 1  = inIntervalSub1 grav done d length0 (last0 + 2)
  | chg        = 0
  | otherwise  = len where
      d          = grav !! (0 + 1)
      first      = until (\x-> x >= d ||      done !! (grav !! (x + 1)))  (+1) 1
      last0      = until (\x-> x >= d || not (done !! (grav !! (x + 2)))) (+1) first
      length0    = last0 - first + 1
      (len, chg) = inIntervalSub2 grav done d False length0 (last0 + 2)

inIntervalSub1 :: [Int] -> [Bool] -> Int -> Int -> Int -> Int
inIntervalSub1 grav done d length0 j
  | j > d                     = length0
  | done !! (grav !! (j + 1)) = 0
  | otherwise                 = inIntervalSub1 grav done d length0 (j + 1)

inIntervalSub2 :: [Int] -> [Bool] -> Int -> Bool -> Int -> Int -> (Int, Bool)
inIntervalSub2 grav done d worried length0 j
  | j > d                                      = (length0, False)
  |      done !! (grav !! (j + 1))             = inIntervalSub2 grav done d True    (length0 + 1) (j + 1)
  | not (done !! (grav !! (j + 1))) && worried = (length0, True)
  | otherwise                                  = inIntervalSub2 grav done d worried length0       (j + 1)



