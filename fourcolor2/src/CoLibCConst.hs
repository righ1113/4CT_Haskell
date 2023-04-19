module CoLibCConst (
  TpAxle,
  TpAxleI,
  TpCond,
  TpAdjmat,
  TpVertices,
  TpGoodConf,
  TpEdgelist,
  TpPosout,
  TpPosoutI,
  TpReducePack,
  TpConfPack,

  TpConfFmt,
  TpAngle,
  TpEdgeNo,
  TpGetENPack,
  TpAnglePack,
  TpExtCJ,
  TpFliveBindPack,
  TpBPSPack,
  TpLiveTwin,
  TpRingNchar,
  TpUpdateState,
  TpUpdateState2,
  TpBaseCol,
  TpTMbind,
  TpRealityPack,
  GConfMajor(..),

  vertsM,
  confs,
  maxval,
  cartvert,
  infty,
  maxoutlets,
  maxelist,
  maxastack,
  maxlev,
  difNouts,

  mverts,
  edgesM,
  maxRing,
  power,
  siMatchNumber,

  debugLogStrip,
  debugLogAngles,
  debugLogUpdateLive,
  (!!|),

  downSym, getPosoutI, outletForced, outletPermitted, reflForced
) where

import Data.Int    ( Int8 )
import Debug.Trace ( trace )

type TpAxle          = ([[Int]], [[Int]], Int)
type TpAxleI         = ([Int], [Int])
type TpCond          = ([Int], [Int])
type TpAdjmat        = [[Int]]
type TpVertices      = [Int]
type TpGoodConf      = ([Int], [Int], [Int], [Int])
type TpEdgelist      = [[[Int]]]
type TpPosout        = ([Int], [Int], [Int], [[Int]], [[Int]], [[Int]])
type TpPosoutI       = (Int, Int, Int, [Int], [Int], [Int])
type TpReducePack    = (TpAxle, [Bool], TpVertices, TpAdjmat, TpEdgelist)
type TpConfPack      = (Bool, Int, [Bool], TpVertices, Int)

type TpConfFmt       = [[Int]]
type TpAngle         = [[Int]]
type TpEdgeNo        = [[Int]]
type TpGetENPack     = (TpConfFmt, Int, Int, [Bool], Int, TpEdgeNo)
type TpAnglePack     = (TpConfFmt, TpEdgeNo, TpAngle, TpAngle, TpAngle, [Int])
type TpExtCJ         = (Bool, [Int], Int)
type TpFliveBindPack = (TpExtCJ, (Int, [Int]), TpExtCJ, TpExtCJ, TpExtCJ, [Int])
type TpBPSPack       = (Int, Int, Int, Int, TpExtCJ, Int)
type TpLiveTwin      = (Int, [Int])
type TpRingNchar     = (Int, Int)
type TpUpdateState   = (TpLiveTwin, [Int], Int, Int8, Int)
type TpUpdateState2  = (TpLiveTwin, [Int], Int, Int8, Int, GConfMajor, TpAnglePack, Bool, Bool)
type TpBaseCol       = (Int, Int, Int)
type TpTMbind        = ([Int], [[Int]], [[[Int]]])
type TpRealityPack   = ([Int], Int, [Int], [Int], Int)
data GConfMajor = Major {
  verts  :: Int,
  ring   :: Int,
  term   :: Int,
  edges  :: Int,
  claim  :: Int,
  cont0  :: Int,
  contE  :: Int,
  bigno  :: Int,
  ncodes :: Int,
  nchar  :: Int }

vertsM        :: Int
vertsM     = 27               -- max number of vertices in a free completion + 1
confs         :: Int
confs      = 640              -- max number of configurations
maxval        :: Int
maxval     = 12
cartvert      :: Int
cartvert   = 5 * maxval + 2   -- domain of l_A, u_A, where A is an axle
infty         :: Int
infty      = 12               -- the "12" in the definition of limited part
maxoutlets    :: Int
maxoutlets = 110              -- max number of outlets
maxelist      :: Int
maxelist   = 134              -- length of edgelist[a][b]
maxastack     :: Int
maxastack  = 5                -- max height of Astack (see "Reduce")
maxlev        :: Int
maxlev     = 12               -- max level of an input line + 1
difNouts      :: [Int]
difNouts   = [0, 0, 0, 0, 0, 0, 0, 103, 64, 53, 53, 53]

mverts        :: Int
mverts     = 27               -- max number of vertices in a free completion + 1
edgesM        :: Int
edgesM      = 62               -- max number of edges in a free completion + 1
maxRing       :: Int
maxRing    = 14               -- max ring-size # 3^(i-1)
power         :: [Int]
power         = [0, 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907]
siMatchNumber :: [Int]
siMatchNumber = [0, 0, 1, 3, 10, 30, 95, 301, 980, 3228, 10797, 36487, 124542, 428506, 1485003]


debugLog :: Bool -> String -> a -> a
debugLog False _ a = a
debugLog True  s a = trace s a

debugLogStrip :: String -> a -> a
debugLogStrip = debugLog True
debugLogAngles :: String -> a -> a
debugLogAngles = debugLog True
debugLogUpdateLive :: String -> a -> a
debugLogUpdateLive = debugLog False

(!!|) :: [a] -> Int -> a
(!!|) xs i = if i >= length xs then last xs else xs !! i
infixl 9 !!|

downSym :: Int -> [Int] -> Int -> Int
downSym nosym nolines lev
  | nosym < 1 || nolines !! (nosym - 1) - 1 < lev = nosym
  | otherwise                                     = downSym (nosym - 1) nolines lev

getPosoutI :: TpPosout -> Int -> TpPosoutI
getPosoutI (num, nol, val, pos, low, upp) i
  = (num !! i, nol !! i, val !! i, pos !! i, low !! i, upp !! i)

outletForced :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
outletForced (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let p1 = posI !! i
              p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
          in if lowI !! i > axLowL !! p2 || uppI !! i < axUppL !! p2 then
            0
          else
            loop1 (i + 1)
  in loop1 0

outletPermitted :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
outletPermitted (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let p1 = posI !! i
              p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
          in if lowI !! i > axUppL !! p2 || uppI !! i < axLowL !! p2 then
            0
          else
            loop1 (i + 1)
  in loop1 0

reflForced :: TpAxleI -> TpPosoutI -> Int -> Int -> Int
reflForced (axLowL, axUppL) (_, nolI, valI, posI, lowI, uppI) pXI deg =
  let
    xxI = pXI - 1
    loop1 i
      | i >= nolI = valI
      | otherwise =
          let
            p1 = posI !! i
            p2 = if xxI + ((p1 - 1) `mod` deg) < deg then p1 + xxI else p1 + xxI - deg
            q
              | p2 <=    deg =     deg - p2 + 1
              | p2 < 2 * deg = 3 * deg - p2
              | otherwise    = 2 * deg
          in if p2 < 1 || p2 > 2 * deg || lowI !! i > axLowL !! q || uppI !! i < axUppL !! q then
            0
          else
            loop1 (i + 1)
  in loop1 0



