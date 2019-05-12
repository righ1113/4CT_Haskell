module Discharge where

import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray, getElems)


verts = 27 -- max number of vertices in a free completion + 1

maxval = 12
cartvert = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

maxelist = 134 -- length of edgelist[a][b]


getQuestion :: TpConfmat -> TpQuestion -> IO ()
getQuestion l q = do
  -- int nverts, max, ring
  -- int d, g, h, i, j, r, t, u, v, w, best, secondbest, search;

  nverts <- readArray l (0, 0)
  q[1].u <- readArray l (0, 0)
  ring <- readArray l (0, 1)
  q[1].v <- readArray l (0, 0)

  found <- newArray (0, verts) False :: IO (IOUArray Int Bool)

  for (max = 0, v = ring + 1; v <= nverts; v++) {
    if (l[v][0] > max) {
      max = l[v][0];
      best = v;
    }
  }
  let q[0].z = best
  q[0].xi <- readArray l (best, 0)
  writeArray found best True

  for (max = 0, i = 1; i <= l[best][0]; i++) {
    v <- readArray l (best, i)
    if (v <= ring)
      continue;
    if (l[v][0] > max) {
      max = l[v][0];
      secondbest = v;
    }
  }
  let q[1].z = secondbest
  q[1].xi <- readArray l (secondbest, 0)
  writeArray found secondbest True

  getQuestion' l q found ring 2 0

getQuestion'Sub1 :: TpConfmat -> TpQuestion -> IO (IOUArray Int Bool) -> Int -> IO Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub1 l q found ring nfound d i v h0
  | h0 == i   = return h0
  | otherwise = do
    u <- readArray l (v, h0)
    if u <= ring
      then
        return h0
      else do
        get <- getElems found
        if (get !! u)
          then
            let h1 = if h0 == 1 then d else h0 - 1
            getQuestion'Sub1 l q found ring nfound d i v h1
          else do
            nf <- readIORef nfound
            let q[nf].z  = u
            lu <- readArray l (u, 0)
            let q[nf].xi = if u > ring then lu else 0
            let stoc     = if h0 == d then 1 else h0 + 1
            q[nf].u <- readArray l (v, stoc)
            let q[nf].v  = v
            modifyIORef nfound (+1)
            writeArray found u True
            let h2 = if h0 == 1 then d else h0 - 1
            getQuestion'Sub1 l q found ring nfound d i v h2

getQuestion'Sub2 :: TpConfmat -> TpQuestion -> IO (IOUArray Int Bool) -> Int -> IO Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub2 l q found ring nfound d i v j0 = do
  -- for (j = (i == d) ? 1 : i + 1;; j = (j == d) ? 1 : j + 1) {
  w <- readArray l (v, j0)
  if w <= ring
    then
      return j0
    else do
      get2 <- getElems found
      if (get2 !! w)
        then
          let j1 = if j0 == d then 1 else j0 + 1
          getQuestion'Sub2 l q found ring nfound d i v j1
        else do
          nf2 <- readIORef nfound
          let q[nf2].z  = w
          lw <- readArray l (w, 0)
          let q[nf2].xi = if w > ring then lw else 0
          let q[nf2].u  = v
          let stoc2     = if j0 == 1 then d else j0 - 1
          lv <- readArray l (v, stoc2)
          let q[nf2].v  = lv
          modifyIORef nfound (+1)
          writeArray found w True
          let j2 = if j0 == d then 1 else j0 + 1
          getQuestion'Sub2 l q found ring nfound d i v j2

getQuestion'Sub3 :: TpConfmat -> TpQuestion -> IO (IOUArray Int Bool) -> Int -> IO Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub3 l q found ring nfound d j v g0
  | g0 == j   = return g0
  | otherwise = do
    -- for (g = (h == 1) ? d : h - 1; g != j; g = (g == 1) ? d : g - 1) {
    t <- readArray l (v, g0)
    get3 <- getElems found
    if (t <= ring) || (get3 !! t)
      then
        error "Error in getquestions"
      else do
        nf4 <- readIORef nfound
        let q[nf4].z = t
        lt <- readArray l (t, 0)
        let q[nf4].xi = if t > ring then lt else 0
        let q[nf4].u = q[nf4 - 1].z
        let q[nf4].v = v
        modifyIORef nfound (+1)
        writeArray found t True
        let g1 = if g0 == 1 then d else g0 - 1
        getQuestion'Sub3 l q found ring nfound d j v g1

getQuestion' :: TpConfmat -> TpQuestion -> IO (IOUArray Int Bool) -> Int -> Int -> Int -> IO ()
getQuestion' l q found ring nfound' search
  | search == nfound' = return ()
  | otherwise         = do

    nfound <- newIORef =<< (return nfound' :: IO Int)

    let v = q[search].z
    if v <= ring
      then
        getQuestion' l q found ring nfound' (search + 1)
      else do

        d <- readArray l (v, 0)

        for (i = 1; !found[l[v][i]]; i++);

        -- サブ関数化
        let h0 = if i == 1 then d else i - 1
        h <- getQuestion'Sub1 l q found ring nfound d i v h0
        if h == i
          then
            getQuestion' l q found ring nfound' (search + 1)
          else do

            -- for (j = (i == d) ? 1 : i + 1;; j = (j == d) ? 1 : j + 1) {
            let j0 = if i == d then 1 else i + 1
            j <- getQuestion'Sub2 l q found ring nfound d i v j0
            let r = if h >= j then h - j else h - j + d
            if r <= 2
              then
                getQuestion' l q found ring nfound' (search + 1)
              else do

                nf3 <- readIORef nfound
                let q[nf3].z = u
                lu <- readArray l (u, 0)
                let q[nf3].xi = if u > ring then lu else 0
                let stac = if h == d then 1 else h + 1
                lv <- readArray l (v, stac)
                let q[nf3].u = lv
                let q[nf3].v = v
                modifyIORef nfound (+1)

                -- for (g = (h == 1) ? d : h - 1; g != j; g = (g == 1) ? d : g - 1) {
                let g0 = if h == 1 then d else h - 1
                g <- getQuestion'Sub3 l q found ring nfound d j v g0
                let q[nfound].u = -1 -- indicates end
                getQuestion' l q found ring nfound' (search + 1)


getEdgelist :: TpAxle -> TpEdgelist -> Int -> IO ()
getEdgelist z edgelist i
  | i == deg + 1 = return ()
  | otherwise    = do
    let deg = (z->upp[0])

    -- static
    for (a = 5; a <= 11; a++)
      for (b = 5; b <= 8 && b <= a; b++)
        edgelist[a][b][0] = 0;
    -- static

    addToList edgelist 0 i (z->upp)
    let h = if i == 1 then deg else i - 1
    addToList edgelist i h (z->upp)
    let a = deg + h
    let b = deg + i
    addToList edgelist i a (z->upp)
    addToList edgelist i b (z->upp)
    if (z->low[i]) /= (z->upp[i])
      then
        getEdgelist z edgelist (i + 1)
      else
        -- in this case we are not interested in the fan edges
        if (z->upp[i]) == 5
          then
            addToList edgelist a b (z->upp)
            getEdgelist z edgelist (i + 1)
          else
            let c = 2 * deg + i
            addToList edgelist a c (z->upp)
            addToList edgelist i c (z->upp)
            if (z->upp[i]) == 6
              then
                addToList edgelist b c (z->upp)
                getEdgelist z edgelist (i + 1)
              else
                let d = 3 * deg + i;
                addToList edgelist c d (z->upp)
                addToList edgelist i d (z->upp)
                if (z->upp[i]) == 7
                  then
                    addToList edgelist b d (z->upp)
                    getEdgelist z edgelist (i + 1)
                  else
                    if (z->upp[i]) /= 8
                      then
                        (void)fflush(stdout);
                        (void)fprintf(stderr, "Unexpected error in `GetEdgeList'\n");
                        exit(36);
                      else
                        let e = 4 * deg + i
                        addToList edgelist d e (z->upp)
                        addToList edgelist i e (z->upp)
                        addToList edgelist b e (z->upp)
                        getEdgelist z edgelist (i + 1)


-- adds the pair u,v to edgelist
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> IO ()
addToList edgelist u v degree = do
  let a = degree !! u
  let b = degree !! v
  if a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
    then do
      eHead <- readArray edgelist (a, b, 0)
      if eHead + 2 >= maxelist
        then
          -- (void) fflush(stdout);
          -- (void) fprintf(stderr, "More than %d entries in edgelist needed\n", maxelist);
          -- exit(39);
          error "More than %d entries in edgelist needed"
        else do
          writeArray edgelist (a, b, 0) (eHead + 1)
          writeArray edgelist (a, b, (eHead + 1)) u
          writeArray edgelist (a, b, 0) (eHead + 2)
          writeArray edgelist (a, b, (eHead + 2)) v
          return ()
    else do
      if b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)
        then do
          eHead2 <- readArray edgelist (b, a, 0)
          if eHead2 + 2 >= maxelist
            then
              -- (void) fflush(stdout);
              -- (void) (stderr, "More than %d entries in edgelist needed\n", maxelist);
              -- exit(41);
              error "More than %d entries in edgelist needed"
            else do
              writeArray edgelist (b, a, 0) (eHead + 1)
              writeArray edgelist (b, a, (eHead + 1)) v
              writeArray edgelist (b, a, 0) (eHead + 2)
              writeArray edgelist (b, a, (eHead + 2)) u
              return ()
        else
          return ()


rootedSubConf :: IOUArray Int Bool -> TpVertices -> TpAdjmat -> TpQuestion -> TpVertices -> Int -> Int -> Int -> Int -> IO Bool
rootedSubConf used degree adjmat question@(qU, qV, qZ, qXi) image x y clockwise j
  | j == head degree + 1 = return True
  | otherwise            = do
    let deg = head degree
    forM_ [0, 1 .. (cartvert-1)] $ \j -> do
      writeArray used j 0
      writeArray image j (-1)
    writeArray image 0 clockwise
    qz0 <- readArray qZ 0
    qz1 <- readArray qZ 1
    writeArray image qZ0 x
    writeArray image qZ1 y
    writeArray used x 1
    writeArray used y 1
    {- わからん
    for (Q = question + 2; Q->u >= 0; Q++) {
      if (clockwise)
        w = adjmat[image[Q->u]][image[Q->v]];
      else
        w = adjmat[image[Q->v]][image[Q->u]];
      if (w == -1)
        return (0);
      if (Q->xi && Q->xi /= degree[w])
        return (0);
      if (used[w])
        return (0);
      image[Q->z] = w;
      used[w] = 1;
    }
    -}
    -- test if image is well-positioned
    let cc = if j == 1 then 2 * deg else deg + j - 1
    usedJ <- readArray used j
    usedD <- readArray used (deg + j)
    usedC <- readArray used cc
    if not usedJ && usedD && usedC
      then
        return False
      else
        rootedSubConf used degree adjmat question image x y clockwise (j+1)


type TpAdjmat = Int
type TpVertices = Int
type TpQuestion = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
type TpEdgelist = IOUArray (Int, Int, Int) Int
--       pedgeHead <- readArray edgelist (qXi0, qXi1, 0)
--    static int used[cartvert]
subConf :: IOUArray Int Bool -> TpAdjmat -> TpVertices -> TpQuestion -> TpEdgelist -> Int -> TpVertices -> Int -> IO Bool
subConf used adjmat degree question@(qU, qV, qZ, qXi) edgelist pedgeHead image i
  | i == pedgeHead + 1 = return False
  | otherwise           = do
    qXi0 <- readArray qXi 0
    qXi1 <- readArray qXi 1
    x <- readArray edgelist (qXi0, qXi1, i+1)
    y <- readArray edgelist (qXi0, qXi1, i)
    first  <- rootedSubConf used degree adjmat question image x y 1 1
    second <- rootedSubConf used degree adjmat question image x y 0 1
    if first || second
      then
        return True
      else
        subConf used adjmat degree question edgelist pedgeHead image (i+1)



