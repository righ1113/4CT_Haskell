{-
◆動かし方
1. https://people.math.gatech.edu/~thomas/OLDFTP/four/
    から「present7」「rules」「unavoidable.conf」を取得し、
    本ファイルと同じ場所に置く。
2. > stack exec ghci --resolver lts
3. > :l Discharge
4. > main
5. > 「7」を入力してEnter。
6. 中心の次数7のグラフは、電荷が負になるか、近くに好配置があらわれるかです
   プログラムは正常終了しました
   が表示されたらOK
-}
module Discharge where

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IORef (IORef(..), newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray)

-- import CsvReader.CsvReader (readCSV)


verts = 27 -- max number of vertices in a free completion + 1

maxval = 12
cartvert = 5 * maxval + 2 -- domain of l_A, u_A, where A is an axle

infty = 12 -- the "12" in the definition of limited part
maxoutlets = 110 -- max number of outlets

maxelist = 134 -- length of edgelist[a][b]

maxlev = 12 -- max level of an input line + 1

type TpConfmat = IOUArray (Int, Int) Int
type TpAxle = (IOUArray (Int, Int) Int, IOUArray (Int, Int) Int, Int)
type TpCond = (IOUArray Int Int, IOUArray Int Int)
type TpAdjmat = IOUArray (Int, Int) Int
type TpVertices = (IOUArray (Int, Int) Int, Int)
type TpQuestion = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
type TpEdgelist = IOUArray (Int, Int, Int) Int
{-typedef struct {
   int number;	/* +/-n for outlet corresponding to rule n, always !=0 */
   int nolines;	/* |M(T)| */
   int value;
   int pos[17];
   int low[17];
   int upp[17];
} tp_outlet;-}
-- type TpOutlet = (IORef Int, IORef Int, IORef Int, IOUArray Int Int, IOUArray Int Int, IOUArray Int Int)
type TpPosout = (IOUArray Int Int, IOUArray Int Int, IOUArray Int Int, IOUArray (Int, Int) Int, IOUArray (Int, Int) Int, IOUArray (Int, Int) Int, IOUArray Int Int)


main :: IO ()
main = do
  putStrLn "これは四色定理の放電法をおこなうプログラムです"
  putStrLn "中心の次数7,8,9,10,11のいずれかを入力してください"
  degStr <- getLine
  let deg = read degStr :: Int

  axlesLow <- newArray ((0, 0), (maxlev + 1, cartvert)) 0 :: IO (IOUArray (Int, Int) Int)
  axlesUpp <- newArray ((0, 0), (maxlev + 1, cartvert)) 0 :: IO (IOUArray (Int, Int) Int)
  writeArray axlesLow (0, 0) deg
  writeArray axlesUpp (0, 0) deg
  forM_ [1, 2 .. (5*deg)] $ \i -> do
    writeArray axlesLow (0, i) 5
    writeArray axlesUpp (0, i) infty
  -- CheckHubcap(axles, NULL, 0, print); -- read rules, compute outlets
  -- (void) Reduce(NULL, 0, 0);          -- read unavoidable set

  -- TpCond
  nn <- newArray (0, maxlev) 0 :: IO (IOUArray Int Int)
  mm <- newArray (0, maxlev) 0 :: IO (IOUArray Int Int)

  -- TpOutlet & TpPosout
  number  <- newArray (0, 2 * maxoutlets) 0 :: IO (IOUArray Int Int)
  nolines <- newArray (0, 2 * maxoutlets) 0 :: IO (IOUArray Int Int)
  value   <- newArray (0, 2 * maxoutlets) 0 :: IO (IOUArray Int Int)
  pos <- newArray ((0,0), (2 * maxoutlets, 16)) 0 :: IO (IOUArray (Int, Int) Int)
  low <- newArray ((0,0), (2 * maxoutlets, 16)) 0 :: IO (IOUArray (Int, Int) Int)
  upp <- newArray ((0,0), (2 * maxoutlets, 16)) 0 :: IO (IOUArray (Int, Int) Int)
  xx      <- newArray (0, 2 * maxoutlets) 0 :: IO (IOUArray Int Int)

  inStr <- readFile $ "present" ++ degStr
  ret <- mainLoop (number, nolines, value, pos, low, upp, xx) (nn, mm) deg (axlesLow, axlesUpp, 0) (tail (map words (lines inStr))) 0

  -- final check
  if ret == "Q.E.D." then
    putStrLn $ "中心の次数" ++ degStr ++ "のグラフは、電荷が負になるか、近くに好配置があらわれるかです"
  else
    putStr ""
  putStrLn "プログラムは正常終了しました"


mainLoop :: TpPosout -> TpCond -> Int -> TpAxle -> [[String]] -> Int -> IO String
mainLoop posout (nn, mm) deg axles@(low, upp, index) tactics lev
  | lev >= maxlev = error "More than %d levels"
  | lev < 0       = return $ head $ head tactics
  | otherwise =

    -- let aA = axles -- &axles[lev]

    {- if (lineno == prtline)
      print = 0;
    lineno = Getstring(str);
    if (lineno == prtline)
      print = printmode;
    if (print >= PRTLIN) {
      (void) printf("%4d:%s", lineno, str);
      fflush(stdout);
    } -}

    -- for (ch = str; *ch == ' '; ch++)	/* do nothing */  ;

    {- if (sscanf(ch, "L%d", &a) != 1 || a != lev) {
      fflush(stdout);
      (void) fprintf(stderr, "Level %d expected on line %d\n", lev, lineno);
      exit(6);
    } -}

    -- for (; *ch != ' '; ch++)	/* do nothing */  ;
    -- for (; *ch == ' '; ch++)	/* do nothing */  ;

    case head tactics !! 1 of
      "S" -> do
              putStrLn "S"
              -- CheckSymmetry(ch, A, sym, nosym, lineno);
              mainLoop posout (nn, mm) deg (low, upp, lev-1) (tail tactics) (lev-1)
      "R" -> do
              putStrLn "R"
              --if (Reduce(A, lineno, print >= PRTBAS ? 1 : 0) != 1) then Error("Reducibility failed", lineno);
              mainLoop posout (nn, mm) deg (low, upp, lev-1) (tail tactics) (lev-1)
      "H" -> do
              putStrLn "H"
              checkHubcap posout (tail (tail (head tactics))) (low, upp, lev)
              mainLoop posout (nn, mm) deg (low, upp, lev-1) (tail tactics) (lev-1)
      "C" -> do
              putStrLn "C"
              let n = read (head tactics !! 2) :: Int
              let m = read (head tactics !! 3) :: Int
              checkCondition (nn, mm) deg (low, upp, lev) n m lev
              mainLoop posout (nn, mm) deg (low, upp, lev+1) (tail tactics) (lev+1)
      _   -> error "Invalid instruction"

    {- /* delete symetries */
    if (print >= PRTBAS && sym[nosym - 1].nolines - 1 >= lev) {
      (void) printf("Deleting symetries:");
      for (i = nosym; i >= 1 && sym[i - 1].nolines - 1 >= lev; i--)
        (void) printf(" %d", sym[i - 1].number);
      (void) printf("\n");
      (void) fflush(stdout);
    } -}

    -- for (; nosym >= 1 && sym[nosym - 1].nolines - 1 >= lev; nosym--) /* do nothing */ ;


-- readRule :: IO ()
-- readRule = print =<< readCSV "rules.csv"
readRule2 :: IO ()
readRule2 = do
  inStr <- readFile "present6"
  print $ map words $ lines inStr


copyAxle :: IOUArray (Int, Int) Int -> IOUArray (Int, Int) Int -> Int -> Int -> IO ()
copyAxle low upp to from =
  forM_ [0, 1 .. cartvert] $ \x -> do
    get1 <- readArray low (from, x)
    get2 <- readArray upp (from, x)
    writeArray low (to, x) get1
    writeArray upp (to, x) get2


checkCondition :: TpCond -> Int -> TpAxle -> Int -> Int -> Int -> IO ()
checkCondition (nn, mm) deg aA@(low, upp, aAI) n m lev = do
  -- check condition and compatibility with A
  -- if (n < 1 || n > 5 * deg)
  --   Error("Invalid vertex in condition", lineno);
  -- if (m < -8 || m > 9 || (m > -5 && m < 6))
  --   Error("Invalid condition", lineno);
  -- let j = (n - 1) `div` deg
  -- let i = (n - 1) `mod` deg + 1
  -- if (n > 2 * deg && (A->low[i] != A->upp[i] || A->low[i] < j + 4))
  --  Error("Condition not compatible with A", lineno);
  copyAxle low upp (aAI + 1) aAI

  aLowN <- readArray low (aAI, n)
  aUppN <- readArray upp (aAI, n)
  if m > 0
    then -- new lower bound
      if aLowN >= m || m > aUppN
        then
          error "Invalid lower bound in condition"
        else do
          writeArray upp (aAI, n) (m - 1)
          writeArray low (aAI + 1, n) m
    else -- new upper bound
      if aLowN > -m || -m >= aUppN
        then
          error "Invalid upper bound in condition"
        else do
          writeArray low (aAI, n) (1 - m)
          writeArray upp (aAI + 1, n) (-m)
{-
  -- remember symmetry unless contains a fan vertex
  good <- newIORef =<< (return True :: IO Bool)
  forM_ [0, 1 .. lev] $ \i -> do
    condIN <- readArray nn i
    if condIN > 2 * deg || condIN < 1
      then
        writeIORef good False
      else
        putStr ""

  if good
    then -- remember symmetry
      --if (*pnosym >= MAXSYM)
      --  Error("Too many symmetries", lineno);
      --if (print >= PRTBAS) {
      --  (void) printf("Adding symmetry:");
      T = &sym[(*pnosym)++];
      T->number = lineno;
      T->value = 1;
      T->nolines = lev + 1;
      for (i = 0; i <= lev; ++i) {
        T->pos[i] = cond[i].n;
        if (cond[i].m > 0) {
          T->low[i] = cond[i].m;
          T->upp[i] = INFTY;
        } else {
          T->low[i] = 5;
          T->upp[i] = -cond[i].m;
        }
        --if (print >= PRTBAS)
        --  (void) printf(" (%d,%d,%d)", T->pos[i], T->low[i], T->upp[i]);
      }
    else
      putStr ""
    -- if (print >= PRTBAS) {
    --(void) printf("Symmetry not added\n");
    --(void) fflush(stdout);
-}
  writeArray nn lev n
  writeArray mm lev m
  writeArray nn (lev+1) 0
  writeArray mm (lev+1) 0


checkHubcap :: TpPosout -> [String] -> TpAxle -> IO ()
checkHubcap posout@(_, _, _, _, _, _, pxx) str aA@(low, upp, aAI) = do
  let xyv = map read str :: [(Int, Int, Int)]
  print xyv

  s <- newArray (0, 2 * maxoutlets + 1) 0 :: IO (IOUArray Int Int)

  {-if (print >= PRTBAS) {
    (void) printf("Testing hubcap for:\n");
    PrintAxle(A);
    (void) printf("Forced positioned outlets:");
    for (i = 1; i <= deg; i++) {
      a = 0;	/* a=1 if edge number printed */
      for (T = outlet, j = 0; j < nouts; ++j, ++T) {}
        if (OutletForced(A, T, i)) {
            if (a == 0) {
              (void) printf("\nEdge %2d: ", i);
              a = 1;
            }
            (void) printf("%2d ", T->number);
        }
      }
    }
    (void) printf("\n");
    (void) fflush(stdout);
  }-}

  {-total = 0;
  for (i = 1; i <= deg; ++i)
    covered[i] = aux[i] = 0;
  for (i = 1; i <= x[0]; ++i) {
    if (x[i] < 1 || x[i] > deg || y[i] < 1 || y[i] > deg) {
      fflush(stdout);
      (void) fprintf(stderr, "Invalid hubcap member (%d,%d,%d)", x[i], y[i], v[i]);
      Error("", lineno);
    }
    if (x[i] == y[i]) {
      total += 2 * v[i];	/* repeated hubcap members listed once */
      if (covered[x[i]])
        Error("Invalid double cover", lineno);
      covered[x[i]] = -1;
    } else {
      total += (aux[x[i]] = v[i]);
      if (covered[x[i]] == -1 || covered[y[i]] == -1)
        Error("Invalid double cover", lineno);
      covered[x[i]] = covered[x[i]] == 0 ? y[i] : -1;
      covered[y[i]] = covered[y[i]] == 0 ? x[i] : -1;
    }
  }-}

  {-for (i = 1; i <= deg; ++i) {
    if (covered[i] == 0)
      Error("Invalid hubcap", lineno);
    if (covered[i] == -1)
      continue;
    if (covered[covered[i]] != i)
      Error("Invalid hubcap", lineno);
    total += aux[i];	/* repeated hubcap members are only listed once */
  }-}

  {-if (print >= PRTBAS)
    (void) printf("Total double cover cost is %d\n", total);
  if (total > 20 * (deg - 6) + 1) {
    fflush(stdout);
    (void) fprintf(stderr, "Double cover has cost %d. ", total);
    Error("Hubcap does not satisfy (H2)", lineno);
  }-}

  let nouts = 103 -- deg = 7
  forM_ [0, 1 .. length str - 1] $ \i -> do
    let (xi, yi, vi) = xyv !! i
    forM_ [0, 1 .. nouts - 1] $ \j ->
      writeArray pxx j xi
      -- s[j] = 0;
    if xi /= yi
      then
        forM_ [nouts, nouts + 1 .. 2 * nouts - 1] $ \j ->
          writeArray pxx j yi
          -- s[j] = 0;
      else
        putStr ""
    writeArray s (2 * nouts) 99 -- to indicate end of list
    runMaybeT $ checkBound aA posout s vi 0 0


checkBound :: TpAxle -> TpPosout -> IOUArray Int Int -> Int -> Int -> Int -> MaybeT IO ()
checkBound aA posout@(_, _, value, _, _, _, pxx) s maxch pos depth = do
  -- compute forced and permitted rules, allowedch, forcedch, update s
  forcedch  <- lift $ newIORef =<< (return 0 :: IO Int)
  allowedch <- lift $ newIORef =<< (return 0 :: IO Int)
  lift $ forM_ [0, 1 .. 99] $ \i -> do
    si     <- readArray s i
    valuei <- readArray value i
    when (si < 99) $
      if si > 0
        then
          modifyIORef forcedch (+valuei)
        else
          if si == 0
            then
              {-if (OutletForced(A, PO->T, PO->x)) {
                s[i] = 1;
                forcedch += PO->T->value;
              } else if (!OutletPermitted(A, PO->T, PO->x))
                s[i] = -1;-}
              if valuei > 0
                then
                  modifyIORef allowedch (+valuei)
                else putStr ""
            else putStr ""

  {-if (print >= PRTPAI) {
    Indent(depth, "POs: ");
    for (i = 0, PO = posout; s[i] < 99; i++, PO++) {
      if (s[i] < 0)
        continue;
      if (s[i] == 0)
        (void) printf("?");
      (void) printf("%d,%d ", PO->T->number, PO->x);
    }
    (void) printf("\n");
  }-}

  -- check if inequality holds
  forcedchV  <- lift $ readIORef forcedch
  allowedchV <- lift $ readIORef allowedch
  if forcedchV + allowedchV <= maxch
    then do
      lift $ putStrLn $ show depth ++ " Inequality holds. Case done."
      -- MaybeT $ return Nothing -- エラーではなく、正常脱出
      fail "" -- エラーではなく、正常脱出
      lift $ putStrLn "come."
    else lift $ putStr ""

  {- -- check reducibility
  if (forcedch > maxch) {
    if (Reduce(A, lineno, print >= PRTALL ? 1 : 0) != 1)
      error "Incorrect hubcap upper bound"
    if (print >= PRTPAI && print < PRTALL)
      putStrLn $ (show depth) ++ " Reducible. Case done."
    return;
  }-}

  lift $ forM_ [0, 1 .. 99] $ \k -> do
    sPos   <- readArray s pos
    valuek <- readArray value k
    when (sPos < 99) $
      if sPos == 0 && valuek >= 0
        then do
          {- x = PO->x;
          -- accepting positioned outlet PO, computing AA
          CopyAxle(AA, A);
          for (i = 0; i < PO->T->nolines; ++i) {
            p = PO->T->pos[i];
            p = x - 1 + (p - 1) % deg < deg ? p + x - 1 : p + x - 1 - deg;
            if (PO->T->low[i] > AA->low[p])
              AA->low[p] = PO->T->low[i];
            if (PO->T->upp[i] < AA->upp[p])
              AA->upp[p] = PO->T->upp[i];
            if (AA->low[p] > AA->upp[p])
              error "Unexpected error 321"
          }	/* i */ -}

          {-/* Check if a previously rejected positioned outlet is forced to apply */
          good = 1;
          for (i = 0; i < pos; i++) {
            if (s[i] == -1 && OutletForced(AA, posout[i].T, posout[i].x)) {
              if (print >= PRTPAI) {
                  Indent(depth, "Positioned outlet ");
                  (void) printf("%d,%d can't be forced, because it forces %d,%d\n", PO->T->number, x, posout[i].T->number, posout[i].x);
              }
              good = 0;
              break;
            }
          }
          if (good) {
            /* recursion with PO forced */
            for (i = 0; (sprime[i] = s[i]) < 99; ++i)	/* do nothing */    ;
            sprime[pos] = 1;
            if (print >= PRTPAI) {
              Indent(depth, "Starting recursion with ");
              (void) printf("%d,%d forced\n", PO->T->number, x);
            }
            CheckBound(AA, posout, sprime, maxch, pos + 1, depth + 1, lineno, print);
          }-}

          -- rejecting positioned outlet PO
          putStrLn "Rejecting positioned outlet "
          writeArray s pos (-1)
          modifyIORef allowedch (+valuek)
          forcedchV2  <- readIORef forcedch
          allowedchV2 <- readIORef allowedch
          if forcedchV2 + allowedchV2 <= maxch
            then do
              putStrLn "Inequality holds."
              liftIO $ fail "rdtlkjewtpwoetj" -- エラーではなく、正常脱出
              putStrLn "come."
            else putStr ""
        else putStr ""

  error "Unexpected error 101"

{-
reduce :: TpAxle -> Int -> MaybeT IO ()
reduce aA naxles
  | naxles <= 0 = lift $ putStrLn "All possibilities for lower degrees tested"
  | otherwise   = do
    naxles2  <- lift $ newIORef =<< (return (naxles-1) :: IO Int)
    naxles2V <- readIORef naxles2
    CopyAxle(B, Astack[naxles2V]);
    {-if (print) {
      (void) printf("Axle from stack:");
      PrintAxle(B);
    }-}
    Getadjmat(B, adjmat);
    GetEdgelist(B, edgelist);
    for (h = 0; h < noconf; ++h)
      if (SubConf(adjmat, B->upp, redquestions[h], edgelist, image))
        break;
    if (h == noconf) {
      putStrLn "Not reducible"
      fail ""
      lift $ putStrLn "come."
    }
    -- Semi-reducibility test found h-th configuration, say K, appearing
    redverts = redquestions[h][1].u;
    redring  = redquestions[h][1].v;
    -- the above are no vertices and ring-size of free completion of K
    -- could not use conf[h][0][0], because conf may be NULL

    {-if (print) {
      (void) printf("Conf(%d,%d,%d): ", h / 70 + 1, (h % 70) / 7 + 1, h % 7 + 1);
      for (j = 1; j <= redverts; j++) {
        if (image[j] != -1)
            (void) printf(" %d(%d)", image[j], j);
      }
      (void) printf("\n");
    }-}
    if (conf != NULL)
      CheckIso(conf[h], B, image, lineno); -- Double-check isomorphism

    for (i = redring + 1; i <= redverts; i++) {
      v = image[i];
      if (B->low[v] == B->upp[v])
        continue;
      naxles4V <- readIORef naxles2
      if naxles4V >= MAXASTACK
        then
          error "More than %d elements in axle stack needed"
        else putStr ""
      CopyAxle(Astack[naxles4V], B);
      Astack[naxles4V]->upp[v] = B->upp[v] - 1;
      modifyIORef naxles2 (+1)
    }

    naxles3V <- readIORef naxles2
    reduce aA naxles3V
/*********************************************************************
            Reduce
 If A==NULL it initializes by allocating memory and reading reducible
 configurations from the file specified by UNAVSET, and computing
 "redquestions" (i.e. questions corresponding to the unavoidable set)
 If A!=NULL it tests reducibility of A as described in [D]
*********************************************************************/
Reduce(A, lineno, print)
int lineno, print;
tp_axle *A;
{
  int h, i, j, v, redring, redverts;
  static int naxles, noconf;
  static tp_confmat *conf;
  static tp_edgelist edgelist;
  static tp_adjmat adjmat;
  static tp_vertices image;
  static tp_axle **Astack, *B;
  static tp_question *redquestions;

  {-if (A == NULL) {
    ALLOC(Astack, MAXASTACK, tp_axle *);
    for (i = 0; i < MAXASTACK; i++)
      ALLOC(Astack[i], 1, tp_axle);
    ALLOC(B, 1, tp_axle);
    redquestions = (tp_question *) malloc(CONFS * sizeof(tp_question));
    if (redquestions == NULL) {
      fflush(stdout);
      (void) fprintf(stderr, "Insufficient memory. Additional %d KBytes needed\n", (int) CONFS * sizeof(tp_question) / 1024);
      exit(27);
    }
    conf = (tp_confmat *) malloc(CONFS * sizeof(tp_confmat));
    if (conf == NULL) {
      (void) printf("Not enough memory to store unavoidable set. Additional %d KBytes needed.\n", (int) CONFS * sizeof(tp_confmat) / 1024);
      (void) printf("Therefore cannot do isomorphism verification.\n");
      fflush(stdout);
    }
    noconf = GetConf(conf, redquestions);
    return (0);
  }-}

  putStrLn "Testing reducibility. Putting input axle on stack."
  CopyAxle(Astack[0], A);

  for (naxles = 1; naxles > 0;) {

  }

  putStrLn "All possibilities for lower degrees tested"
  --return (1);
}/* Reduce */
-}


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
getEdgelist z@(zLow, zUpp, zIndex) deg edgelist i
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

    addToList edgelist 0 i (zUpp, zIndex)
    let h = if i == 1 then deg else i - 1
    addToList edgelist i h (zUpp, zIndex)
    let a = deg + h
    let b = deg + i
    addToList edgelist i a (zUpp, zIndex)
    addToList edgelist i b (zUpp, zIndex)
    zLowI <- readArray zLow (zIndex, i)
    zUppI <- readArray zUpp (zIndex, i)
    if zLowI /= zUppI
      then
        getEdgelist z deg edgelist (i + 1)
      else
        -- in this case we are not interested in the fan edges
        if zUppI == 5
          then do
            addToList edgelist a b (zUpp, zIndex)
            getEdgelist z deg edgelist (i + 1)
          else do
            let c = 2 * deg + i
            addToList edgelist a c (zUpp, zIndex)
            addToList edgelist i c (zUpp, zIndex)
            if zUppI == 6
              then do
                addToList edgelist b c (zUpp, zIndex)
                getEdgelist z deg edgelist (i + 1)
              else do
                let d = 3 * deg + i;
                addToList edgelist c d (zUpp, zIndex)
                addToList edgelist i d (zUpp, zIndex)
                if zUppI == 7
                  then do
                    addToList edgelist b d (zUpp, zIndex)
                    getEdgelist z deg edgelist (i + 1)
                  else
                    if zUppI == 8
                      then do
                        let e = 4 * deg + i
                        addToList edgelist d e (zUpp, zIndex)
                        addToList edgelist i e (zUpp, zIndex)
                        addToList edgelist b e (zUpp, zIndex)
                        getEdgelist z deg edgelist (i + 1)
                      else
                        -- (void)fflush(stdout);
                        -- (void)fprintf(stderr, "Unexpected error in `GetEdgeList'\n");
                        -- exit(36);
                        error "Unexpected error in `GetEdgeList'"


-- adds the pair u,v to edgelist
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> IO ()
addToList edgelist u v (degree, dIndex) = do
  a <- readArray degree (dIndex, u)
  b <- readArray degree (dIndex, v)
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


rootedSubConf :: IOUArray Int Bool -> TpVertices -> Int -> TpAdjmat -> TpQuestion -> TpVertices -> Int -> Int -> Int -> Int -> MaybeT IO ()
rootedSubConf used (degree, dI) deg adjmat question@(qU, qV, qZ, qXi) (image, iI) x y clockwise j
  | j == deg + 1 = lift $ return ()
  | otherwise            = do
    lift $ forM_ [0, 1 .. (cartvert-1)] $ \j -> do
      writeArray used j False
      writeArray image (iI, j) (-1)
    lift $ writeArray image (iI, 0) clockwise
    qZ0 <- lift $ readArray qZ 0
    qZ1 <- lift $ readArray qZ 1
    lift $ writeArray image (iI, qZ0) x
    lift $ writeArray image (iI, qZ1) y
    lift $ writeArray used x True
    lift $ writeArray used y True

    lift $ forM_ [2, 3 .. 1024] $ \q -> do
      qUQ <- readArray qU q
      qVQ <- readArray qV q
      qZQ <- readArray qZ q
      qXiQ <- readArray qXi q
      imageQUQ <- readArray image (iI, qUQ)
      imageQVQ <- readArray image (iI, qVQ)
      if qUQ < 0
        then
          liftIO $ fail ""
        else do
          w <- if clockwise /= 0
                 then readArray adjmat (imageQUQ, imageQVQ)
                 else readArray adjmat (imageQVQ, imageQUQ)
          if w == -1
            then
              liftIO $ fail ""
            else do
              degreeW <- readArray degree (dI, w)
              if qXiQ /= 0 && qXiQ /= degreeW
                then
                  liftIO $ fail ""
                else do
                  usedW <- readArray used w
                  if usedW
                    then
                      liftIO $ fail ""
                    else do
                      writeArray image (iI, qZQ) w
                      writeArray used w True

    -- test if image is well-positioned
    let cc = if j == 1 then 2 * deg else deg + j - 1
    usedJ <- lift $ readArray used j
    usedD <- lift $ readArray used (deg + j)
    usedC <- lift $ readArray used cc
    if not usedJ && usedD && usedC
      then
        lift $ fail ""
      else
        rootedSubConf used (degree, dI) deg adjmat question (image, iI) x y clockwise (j+1)


--       pedgeHead <- readArray edgelist (qXi0, qXi1, 0)
--    static int used[cartvert]
subConf :: IOUArray Int Bool -> TpAdjmat -> TpVertices -> TpQuestion -> TpEdgelist -> Int -> TpVertices -> Int -> IO Bool
subConf used adjmat (degree, dI) question@(qU, qV, qZ, qXi) edgelist pedgeHead image i
  | i == pedgeHead + 1 = return False
  | otherwise           = do
    qXi0 <- readArray qXi 0
    qXi1 <- readArray qXi 1
    x <- readArray edgelist (qXi0, qXi1, i+1)
    y <- readArray edgelist (qXi0, qXi1, i)
    headD <- readArray degree (dI, 0)
    first  <- runMaybeT $ rootedSubConf used (degree, dI) headD adjmat question image x y 1 1
    second <- runMaybeT $ rootedSubConf used (degree, dI) headD adjmat question image x y 0 1
    if first == Just () || second == Just ()
      then
        return True
      else
        subConf used adjmat (degree, dI) question edgelist pedgeHead image (i+1)



