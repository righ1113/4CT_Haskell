module Discharge where

import Control.Monad (forM_, when, unless)
import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray)


verts = 27 -- max number of vertices in a free completion + 1

maxval = 12
cartvert = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

maxelist = 134 -- length of edgelist[a][b]

type TpConfmat = IOUArray (Int, Int) Int
type TpAxle = (IOUArray Int Int, IOUArray Int Int)
type TpAdjmat = IOUArray (Int, Int) Int
type TpVertices = IOUArray Int Int
type TpQuestion = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
type TpEdgelist = IOUArray (Int, Int, Int) Int


getQuestion :: TpConfmat -> TpQuestion -> IO ()
getQuestion l q@(qU, qV, qZ, qXi) = do
  -- int nverts, max, ring
  -- int d, g, h, i, j, r, t, u, v, w, best, secondbest, search;

  nverts <- readArray l (0, 0)
  writeArray qU 1 nverts
  ring <- readArray l (0, 1)
  writeArray qV 1 ring

  found <- newArray (0, verts) False :: IO (IOUArray Int Bool)

  max <- newIORef =<< (return 0 :: IO Int)
  best <- newIORef =<< (return 0 :: IO Int)
  forM_ [(ring+1), (ring+2) .. nverts] $ \v -> do
    lV0 <- readArray l (v, 0)
    maxV <- readIORef max
    if lV0 > maxV
      then do
        writeIORef max lV0
        writeIORef best v
      else
        putStr ""
  bestV <- readIORef best
  writeArray qZ 0 bestV
  stoc <- readArray l (bestV, 0)
  writeArray qXi 0 stoc
  writeArray found bestV True

  max2 <- newIORef =<< (return 0 :: IO Int)
  secondBest <- newIORef =<< (return 0 :: IO Int)
  lBest <- readArray l (bestV, 0)
  forM_ [1, 2 .. lBest] $ \i -> do
    v <- readArray l (bestV, i)
    lV0 <- readArray l (v, 0)
    maxV2 <- readIORef max2
    if v <= ring
      then
        putStr ""
      else
        if lV0 > maxV2
          then do
            writeIORef max2 lV0
            writeIORef secondBest v
          else
            putStr ""
  secondBestV <- readIORef secondBest
  writeArray qZ 1 secondBestV
  stoc2 <- readArray l (secondBestV, 0)
  writeArray qXi 1 stoc2
  writeArray found secondBestV True

  getQuestion' l q found ring 2 0


getQuestion' :: TpConfmat -> TpQuestion -> IOUArray Int Bool -> Int -> Int -> Int -> IO ()
getQuestion' l q@(qU, qV, qZ, qXi) found ring nfound' search
  | search == nfound' = return ()
  | otherwise         = do

    nfound <- newIORef =<< (return nfound' :: IO Int)
    ifound <- newIORef =<< (return 1 :: IO Int)

    v <- readArray qZ search
    if v <= ring
      then
        getQuestion' l q found ring nfound' (search + 1)
      else do

        d <- readArray l (v, 0)

        forM_ [1, 2 .. 1024] $ \ii -> do
          lVII <- readArray l (v, ii)
          fo <- readArray found lVII
          unless fo $
            modifyIORef ifound (+1)
        i <- readIORef ifound

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

                u <- readArray l (v, h)
                nf3 <- readIORef nfound
                writeArray qZ nf3 u
                lu <- readArray l (u, 0)
                writeArray qXi nf3 (if u > ring then lu else 0)
                let stac = if h == d then 1 else h + 1
                lv <- readArray l (v, stac)
                writeArray qU nf3 lv
                writeArray qV nf3 v
                modifyIORef nfound (+1)

                -- for (g = (h == 1) ? d : h - 1; g != j; g = (g == 1) ? d : g - 1) {
                let g0 = if h == 1 then d else h - 1
                g <- getQuestion'Sub3 l q found ring nfound d j v g0
                nf4 <- readIORef nfound
                writeArray qU nf4 (-1) -- indicates end
                getQuestion' l q found ring nfound' (search + 1)

getQuestion'Sub1 :: TpConfmat -> TpQuestion -> IOUArray Int Bool -> Int -> IORef Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub1 l q@(qU, qV, qZ, qXi) found ring nfound d i v h0
  | h0 == i   = return h0
  | otherwise = do
    u <- readArray l (v, h0)
    if u <= ring
      then
        return h0
      else do
        get <- readArray found u
        if get
          then do
            let h1 = if h0 == 1 then d else h0 - 1
            getQuestion'Sub1 l q found ring nfound d i v h1
          else do
            nf <- readIORef nfound
            writeArray qZ nf u
            lu <- readArray l (u, 0)
            writeArray qXi nf (if u > ring then lu else 0)
            let stoc     = if h0 == d then 1 else h0 + 1
            stoc2 <- readArray l (v, stoc)
            writeArray qU nf stoc2
            writeArray qV nf v
            modifyIORef nfound (+1)
            writeArray found u True
            let h2 = if h0 == 1 then d else h0 - 1
            getQuestion'Sub1 l q found ring nfound d i v h2

getQuestion'Sub2 :: TpConfmat -> TpQuestion -> IOUArray Int Bool -> Int -> IORef Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub2 l q@(qU, qV, qZ, qXi) found ring nfound d i v j0 = do
  -- for (j = (i == d) ? 1 : i + 1;; j = (j == d) ? 1 : j + 1) {
  w <- readArray l (v, j0)
  if w <= ring
    then
      return j0
    else do
      get2 <- readArray found w
      if get2
        then do
          let j1 = if j0 == d then 1 else j0 + 1
          getQuestion'Sub2 l q found ring nfound d i v j1
        else do
          nf2 <- readIORef nfound
          writeArray qZ nf2 w
          lw <- readArray l (w, 0)
          writeArray qXi nf2 (if w > ring then lw else 0)
          writeArray qU nf2 v
          let stoc2     = if j0 == 1 then d else j0 - 1
          lv <- readArray l (v, stoc2)
          writeArray qV nf2 lv
          modifyIORef nfound (+1)
          writeArray found w True
          let j2 = if j0 == d then 1 else j0 + 1
          getQuestion'Sub2 l q found ring nfound d i v j2

getQuestion'Sub3 :: TpConfmat -> TpQuestion -> IOUArray Int Bool -> Int -> IORef Int -> Int -> Int -> Int -> Int -> IO Int
getQuestion'Sub3 l q@(qU, qV, qZ, qXi) found ring nfound d j v g0
  | g0 == j   = return g0
  | otherwise = do
    -- for (g = (h == 1) ? d : h - 1; g != j; g = (g == 1) ? d : g - 1) {
    t <- readArray l (v, g0)
    get3 <- readArray found t
    if (t <= ring) || get3
      then
        error "Error in getquestions"
      else do
        nf4 <- readIORef nfound
        writeArray qZ nf4 t
        lt <- readArray l (t, 0)
        writeArray qXi nf4 (if t > ring then lt else 0)
        stoc <- readArray qZ (nf4 - 1)
        writeArray qU nf4 stoc
        writeArray qV nf4 v
        modifyIORef nfound (+1)
        writeArray found t True
        let g1 = if g0 == 1 then d else g0 - 1
        getQuestion'Sub3 l q found ring nfound d j v g1


--    let deg = (z->upp[0])
getEdgelist :: TpAxle -> Int -> TpEdgelist -> Int -> IO ()
getEdgelist z@(zLow, zUpp) deg edgelist i
  | i == deg + 1 = return ()
  | otherwise    = do
    if i == 1
      then
        forM_ [5, 6 .. 11] $ \a ->
          forM_ [5, 6 .. 8] $ \b ->
            when (b <= a) $
              writeArray edgelist (a, b, 0) 0
      else
        putStr ""

    addToList edgelist 0 i zUpp
    let h = if i == 1 then deg else i - 1
    addToList edgelist i h zUpp
    let a = deg + h
    let b = deg + i
    addToList edgelist i a zUpp
    addToList edgelist i b zUpp
    zLowI <- readArray zLow i
    zUppI <- readArray zUpp i
    if zLowI /= zUppI
      then
        getEdgelist z deg edgelist (i + 1)
      else
        -- in this case we are not interested in the fan edges
        if zUppI == 5
          then do
            addToList edgelist a b zUpp
            getEdgelist z deg edgelist (i + 1)
          else do
            let c = 2 * deg + i
            addToList edgelist a c zUpp
            addToList edgelist i c zUpp
            if zUppI == 6
              then do
                addToList edgelist b c zUpp
                getEdgelist z deg edgelist (i + 1)
              else do
                let d = 3 * deg + i;
                addToList edgelist c d zUpp
                addToList edgelist i d zUpp
                if zUppI == 7
                  then do
                    addToList edgelist b d zUpp
                    getEdgelist z deg edgelist (i + 1)
                  else
                    if zUppI == 8
                      then do
                        let e = 4 * deg + i
                        addToList edgelist d e zUpp
                        addToList edgelist i e zUpp
                        addToList edgelist b e zUpp
                        getEdgelist z deg edgelist (i + 1)
                      else
                        -- (void)fflush(stdout);
                        -- (void)fprintf(stderr, "Unexpected error in `GetEdgeList'\n");
                        -- exit(36);
                        error "Unexpected error in `GetEdgeList'"


-- adds the pair u,v to edgelist
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> IO ()
addToList edgelist u v degree = do
  a <- readArray degree u
  b <- readArray degree v
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
          writeArray edgelist (a, b, eHead + 1) u
          writeArray edgelist (a, b, 0) (eHead + 2)
          writeArray edgelist (a, b, eHead + 2) v
          return ()
    else
      when (b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)) $ do
        eHead2 <- readArray edgelist (b, a, 0)
        if eHead2 + 2 >= maxelist
          then
            -- (void) fflush(stdout);
            -- (void) (stderr, "More than %d entries in edgelist needed\n", maxelist);
            -- exit(41);
            error "More than %d entries in edgelist needed"
          else do
            writeArray edgelist (b, a, 0) (eHead2 + 1)
            writeArray edgelist (b, a, eHead2 + 1) v
            writeArray edgelist (b, a, 0) (eHead2 + 2)
            writeArray edgelist (b, a, eHead2 + 2) u
            return ()


rootedSubConf :: IOUArray Int Bool -> TpVertices -> Int -> TpAdjmat -> TpQuestion -> TpVertices -> Int -> Int -> Int -> Int -> IO Bool
rootedSubConf used degree deg adjmat question@(qU, qV, qZ, qXi) image x y clockwise j
  | j == deg + 1 = return True
  | otherwise            = do
    forM_ [0, 1 .. (cartvert-1)] $ \j -> do
      writeArray used j False
      writeArray image j (-1)
    writeArray image 0 clockwise
    qZ0 <- readArray qZ 0
    qZ1 <- readArray qZ 1
    writeArray image qZ0 x
    writeArray image qZ1 y
    writeArray used x True
    writeArray used y True
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
        rootedSubConf used degree deg adjmat question image x y clockwise (j+1)


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
    headD <- readArray degree 0
    first  <- rootedSubConf used degree headD adjmat question image x y 1 1
    second <- rootedSubConf used degree headD adjmat question image x y 0 1
    if first || second
      then
        return True
      else
        subConf used adjmat degree question edgelist pedgeHead image (i+1)



