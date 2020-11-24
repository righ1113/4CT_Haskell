module DiLibConst where

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

verts      = 27 :: Int               -- max number of vertices in a free completion + 1
confs      = 640 :: Int              -- max number of configurations
maxval     = 12 :: Int
cartvert   = (5 * maxval + 2) :: Int -- domain of l_A, u_A, where A is an axle
infty      = 12 :: Int               -- the "12" in the definition of limited part
maxoutlets = 110 :: Int              -- max number of outlets
maxelist   = 134 :: Int              -- length of edgelist[a][b]
maxastack  = 5 :: Int                -- max height of Astack (see "Reduce")
maxlev     = 12 :: Int               -- max level of an input line + 1
difNouts   = [0, 0, 0, 0, 0, 0, 0, 103, 103, 103, 103, 103] :: [Int]



