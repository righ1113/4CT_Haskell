module CoLibCConst
        (
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

          TpConfmat,
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

          verts,
          confs,
          maxval,
          cartvert,
          infty,
          maxoutlets,
          maxelist,
          maxastack,
          maxlev,
          difNouts,

          edges,
          maxRing,
          power,
          siMatchNumber,

          readFileGoodConfsR,
          debugLogStrip,
          debugLogAngles,
          debugLogUpdateLive
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

type TpConfmat       = [[Int]]
type TpAngle         = [[Int]]
type TpEdgeNo        = [[Int]]
type TpGetENPack     = (TpConfmat, Int, Int, [Bool], Int, TpEdgeNo)
type TpAnglePack     = (TpConfmat, TpEdgeNo, TpAngle, TpAngle, TpAngle, [Int])
type TpExtCJ         = (Bool, [Int], Int)
type TpFliveBindPack = (TpExtCJ, (Int, [Int]), TpExtCJ, TpExtCJ, TpExtCJ, [Int])
type TpBPSPack       = (Int, Int, Int, Int, TpExtCJ, Int)
type TpLiveTwin      = (Int, [Int])
type TpRingNchar     = (Int, Int)
type TpUpdateState   = (TpLiveTwin, [Int], Int, Int8, Int)
type TpUpdateState2  = (TpLiveTwin, [Int], Int, Int8, Int, TpRingNchar)
type TpBaseCol       = (Int, Int, Int)
type TpTMbind        = ([Int], [[Int]], [[[Int]]])
type TpRealityPack   = ([Int], Int, [Int], [Int], Int)

verts         :: Int
verts      = 27               -- max number of vertices in a free completion + 1
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

-- mverts     = 27 :: Int               -- max number of vertices in a free completion + 1
edges         :: Int
edges      = 62               -- max number of edges in a free completion + 1
maxRing       :: Int
maxRing    = 14               -- max ring-size # 3^(i-1)
power         :: [Int]
power         = [0, 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907]
siMatchNumber :: [Int]
siMatchNumber = [0, 0, 1, 3, 10, 30, 95, 301, 980, 3228, 10797, 36487, 124542, 428506, 1485003]


readFileGoodConfsR :: IO [TpConfmat]
readFileGoodConfsR = do
  unaStr <- readFile "4ctdata/ReGoodConfs.txt"
  return (read unaStr :: [TpConfmat])

debugLog :: Bool -> String -> a -> a
debugLog False _ a = a
debugLog True  s a = trace s a

debugLogStrip :: String -> a -> a
debugLogStrip = debugLog False
debugLogAngles :: String -> a -> a
debugLogAngles = debugLog False
debugLogUpdateLive :: String -> a -> a
debugLogUpdateLive = debugLog False



