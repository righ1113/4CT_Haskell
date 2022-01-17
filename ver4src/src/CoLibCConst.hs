module CoLibCConst where

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
type TpEdgeno        = [[Int]]
type TpGetENPack     = (TpConfmat, Int, Int, [Bool], Int, TpEdgeno)
type TpAnglePack     = (TpConfmat, TpEdgeno, TpAngle, TpAngle, TpAngle, [Int])

verts      = 27 :: Int               -- max number of vertices in a free completion + 1
confs      = 640 :: Int              -- max number of configurations
maxval     = 12 :: Int
cartvert   = (5 * maxval + 2) :: Int -- domain of l_A, u_A, where A is an axle
infty      = 12 :: Int               -- the "12" in the definition of limited part
maxoutlets = 110 :: Int              -- max number of outlets
maxelist   = 134 :: Int              -- length of edgelist[a][b]
maxastack  = 5 :: Int                -- max height of Astack (see "Reduce")
maxlev     = 12 :: Int               -- max level of an input line + 1
difNouts   = [0, 0, 0, 0, 0, 0, 0, 103, 64, 53, 53, 53] :: [Int]

mverts     = 27 :: Int               -- max number of vertices in a free completion + 1
edges      = 62 :: Int               -- max number of edges in a free completion + 1
power         = [0, 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907] :: [Int]
simatchnumber = [0, 0, 1, 3, 10, 30, 95, 301, 980, 3228, 10797, 36487, 124542, 428506, 1485003] :: [Int]

readFileGoodConfsR :: IO [TpConfmat]
readFileGoodConfsR = do
  unaStr <- readFile "4ctdata/ReGoodConfs.txt"
  return (read unaStr :: [TpConfmat])



