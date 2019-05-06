module Discharge where

import Data.Array.IO (IOUArray(..), newListArray, writeArray, getElems)

main = do
  a <- newListArray (0,2) [1,2,3] :: IO (IOUArray Int Int)
  print =<< getElems a

  writeArray a 0 9
  print =<< getElems a

  writeArray a 2 9
  get <- getElems a
  print $ get !! 2


{-
/**********************************************************************
	GetEdgeList
For (a,b) such that a >= b, b <= 8 and a <= 11 computes X=edgelist[a][b]
defined as follows: X[2*i+1],X[2*i+2] (i=0,1,...,X[0]-1) are all pairs of
adjacent vertices u,v of the skeleton of A with degrees a,b, respectively
such that either a<=8 or u=0.
***********************************************************************/
void
GetEdgelist(A, edgelist)
tp_axle *A;
tp_edgelist edgelist;
{
   int a, b, c, d, e, h, i, deg;
   deg = A->upp[0];
   for (a = 5; a <= 11; a++)
      for (b = 5; b <= 8 && b <= a; b++)
	 edgelist[a][b][0] = 0;
   for (i = 1; i <= deg; i++) {
      AddToList(edgelist, 0, i, A->upp);
      h = (i == 1) ? deg : i - 1;
      AddToList(edgelist, i, h, A->upp);
      a = deg + h;
      b = deg + i;
      AddToList(edgelist, i, a, A->upp);
      AddToList(edgelist, i, b, A->upp);
      if (A->low[i] != A->upp[i])
	 continue;
      /* in this case we are not interested in the fan edges */
      if (A->upp[i] == 5) {
	 AddToList(edgelist, a, b, A->upp);
	 continue;
      }
      c = 2 * deg + i;
      AddToList(edgelist, a, c, A->upp);
      AddToList(edgelist, i, c, A->upp);
      if (A->upp[i] == 6) {
	 AddToList(edgelist, b, c, A->upp);
	 continue;
      }
      d = 3 * deg + i;
      AddToList(edgelist, c, d, A->upp);
      AddToList(edgelist, i, d, A->upp);
      if (A->upp[i] == 7) {
	 AddToList(edgelist, b, d, A->upp);
	 continue;
      }
      if (A->upp[i] != 8) {
	 (void) fflush(stdout);
	 (void) fprintf(stderr, "Unexpected error in `GetEdgeList'\n");
	 exit(36);
      }
      e = 4 * deg + i;
      AddToList(edgelist, d, e, A->upp);
      AddToList(edgelist, i, e, A->upp);
      AddToList(edgelist, b, e, A->upp);
   }
}/* GetEdgeList */
-}
getEdgelist :: TpAxle -> TpEdgelist -> Int -> IO ()
getEdgelist z edgelist i
  | i == deg + 1 = return ()
  | otherwise = do
    let deg = (z->upp[0])

    -- static
    for (a = 5; a <= 11; a++)
      for (b = 5; b <= 8 && b <= a; b++)
        edgelist[a][b][0] = 0;
    -- static

    AddToList edgelist 0 i (z->upp)
    let h = if i == 1 then deg else i - 1
    AddToList edgelist i h (z->upp)
    let a = deg + h
    let b = deg + i
    AddToList edgelist i a (z->upp)
    AddToList edgelist i b (z->upp)

    if (z->low[i]) /= (z->upp[i])
      then
        getEdgelist z edgelist (i + 1)
      else
        -- in this case we are not interested in the fan edges
        if (z->upp[i]) == 5
          then
            AddToList edgelist a b (z->upp)
            getEdgelist z edgelist (i + 1)
          else
            let c = 2 * deg + i
            AddToList edgelist a c (z->upp)
            AddToList edgelist i c (z->upp)
            if (z->upp[i]) == 6
              then
                AddToList edgelist b c (z->upp)
                getEdgelist z edgelist (i + 1)
              else
                let d = 3 * deg + i;
                AddToList edgelist c d (z->upp)
                AddToList edgelist i d (z->upp)
                if (z->upp[i]) == 7
                  then
                    AddToList edgelist b d (z->upp)
                    getEdgelist z edgelist (i + 1)
                  else
                    if (z->upp[i]) /= 8
                      then
                        (void)fflush(stdout);
                        (void)fprintf(stderr, "Unexpected error in `GetEdgeList'\n");
                        exit(36);
                      else
                        let e = 4 * deg + i
                        AddToList edgelist d e (z->upp)
                        AddToList edgelist i e (z->upp)
                        AddToList edgelist b e (z->upp)
                        getEdgelist z edgelist (i + 1)


{-
/**********************************************************************
	AddToList
See "GetEdgeList" above.
***********************************************************************/
void
AddToList(edgelist, u, v, degree)
tp_edgelist edgelist;
int u, v;
tp_vertices degree;
/* adds the pair u,v to edgelist */
{
   int a, b, *e;

   a = degree[u];
   b = degree[v];
   if ((a >= b) && (b <= 8) && (a <= 11) && ((a <= 8) || (u == 0))) {
      e = edgelist[a][b];
      if (e[0] + 2 >= MAXELIST) {
	 (void) fflush(stdout);
	 (void) fprintf(stderr, "More than %d entries in edgelist needed\n", MAXELIST);
	 exit(39);
      }
      e[++e[0]] = u;
      e[++e[0]] = v;
   }
   if ((b >= a) && (a <= 8) && (b <= 11) && ((b <= 8) || (v == 0))) {
      e = edgelist[b][a];
      if (e[0] + 2 >= MAXELIST) {
	 (void) fflush(stdout);
	 (void) (stderr, "More than %d entries in edgelist needed\n", MAXELIST);
	 exit(41);
      }
      e[++e[0]] = v;
      e[++e[0]] = u;
   }
}
-}
addToList :: TpEdgelist -> Int -> Int -> TpVertices -> IO ()
-- adds the pair u,v to edgelist
addToList edgelist u v degree = do
  let a = degree !! u
  let b = degree !! v
  if a >= b && b <= 8 && a <= 11 && (a <= 8 || u == 0)
    then
      let e = edgelist[a][b]
      if head e + 2 >= MAXELIST
        then
          (void) fflush(stdout);
          (void) fprintf(stderr, "More than %d entries in edgelist needed\n", MAXELIST);
          exit(39);
        else
          putStr ""
      e[++e[0]] = u;
      e[++e[0]] = v;
    else
      if b >= a && a <= 8 && b <= 11 && (b <= 8 || v == 0)
        then
          e = edgelist[b][a];
          if head e + 2 >= MAXELIST
            then
              (void) fflush(stdout);
              (void) (stderr, "More than %d entries in edgelist needed\n", MAXELIST);
              exit(41);
            else
              putStr ""
          e[++e[0]] = v;
          e[++e[0]] = u;
        else
          return ()



{-
/**********************************************************************
	RootedSubConf
See "SubConf" below.
***********************************************************************/
RootedSubConf(degree, adjmat, question, image, x, y, clockwise)
int degree[], x, y, clockwise;
tp_adjmat adjmat;
tp_vertices image;
tp_question question;
{
   int deg, j, w;
   static int used[CARTVERT];
   tp_query *Q;

   deg = degree[0];
   for (j = 0; j < CARTVERT; j++) {
      used[j] = 0;
      image[j] = -1;
   }
   image[0] = clockwise;
   image[question[0].z] = x;
   image[question[1].z] = y;
   used[x] = 1;
   used[y] = 1;
   for (Q = question + 2; Q->u >= 0; Q++) {
      if (clockwise)
	 w = adjmat[image[Q->u]][image[Q->v]];
      else
	 w = adjmat[image[Q->v]][image[Q->u]];
      if (w == -1)
	 return (0);
      if (Q->xi && Q->xi != degree[w])
	 return (0);
      if (used[w])
	 return (0);
      image[Q->z] = w;
      used[w] = 1;
   }
   /* test if image is well-positioned */
   for (j = 1; j <= deg; j++)
      if (!used[j] && used[deg + j] && used[(j == 1) ? 2 * deg : deg + j - 1])
	 return (0);
   return (1);
}/* RootedSubConf */
-}
rootedSubConf :: TpVertices -> TpAdjmat -> TpQuestion -> TpVertices -> Int -> Int -> Int -> Int -> IO Bool
rootedSubConf degree adjmat question image x y clockwise j
  | j == head degree + 1 = return True
  | otherwise = do
    static int used[CARTVERT]
    let deg = head degree
    forM_ [0, 1 .. (CARTVERT-1)] $ \j ->
      writeArray used j 0
    forM_ [0, 1 .. (CARTVERT-1)] $ \j ->
      writeArray image j (-1)
    writeArray image 0 clockwise
    writeArray image question[0].z x
    writeArray image question[1].z y
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
    if not $ used !! j && used !! (deg + j) && used !! cc
      then
        return False
      else
        rootedSubConf degree adjmat question image x y clockwise (j+1)


{-
/**********************************************************************
	SubConf
Given "adjmat", "degree" and "edgelist" derived from an axle A, and
"question" for a configuration L it tests using [D, theorem (6.3)]
if L is a well-positioned induced subconfiguration of the skeleton
of A. If not returns 0; otherwise returns 1, writes an isomorphism
into image, and sets image[0] to 1 if the isomorphism is orientation-
preserving, and 0 if it is orientation-reversing.
***********************************************************************/
SubConf(adjmat, degree, question, edgelist, image)
tp_adjmat adjmat;
tp_vertices degree;
tp_edgelist edgelist;
tp_vertices image;
tp_question question;
{
   int i, x, y, *pedge;

   pedge = edgelist[question[0].xi][question[1].xi];
   for (i = 1; i <= pedge[0]; i++) {
      x = pedge[i++];
      y = pedge[i];
      if (RootedSubConf(degree, adjmat, question, image, x, y, 1) ||
	  RootedSubConf(degree, adjmat, question, image, x, y, 0))
	 return (1);
   }
   return (0);
}/* SubConf */
-}
subConf :: TpAdjmat -> TpVertices -> TpQuestion -> TpEdgelist -> TpVertices -> Int -> IO Bool
subConf adjmat degree question edgelist image i
  | i == head pedge + 1 = return False
  | otherwise = do
    --let pedge = edgelist[question[0].xi][question[1].xi]
    let x = pedge !! (i+1)
    let y = pedge !! i
    first  <- rootedSubConf degree adjmat question image x y 1 1
    second <- rootedSubConf degree adjmat question image x y 0 1
    if first || second
      then
        return True
      else
        subConf adjmat degree question edgelist image (i+1)



