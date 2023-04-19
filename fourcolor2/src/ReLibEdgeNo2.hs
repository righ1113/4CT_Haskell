module ReLibEdgeNo2( getEdgeNo ) where

import CoLibCConst
    ( TpEdgeNo, TpConfFmt, debugLogStrip, edgesM )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.List     ( sortOn )
import Data.Ord      ( Down(..) )


getEdgeNo :: Int -> Int -> Int -> TpConfFmt -> TpEdgeNo
getEdgeNo vertex ring term gConf = debugLogStrip ("$$$ vertex: " ++ show edgeNo) edgeNo where
  edgeNo    = getEdgeNoSub2 edgeList2 term . getEdgeNoSub1 edgeList1 $ edgeNo0
  edgeNo0   = newEdgeNo 1 ring (replicate edgesM $ replicate edgesM 0)
  edgeList1 = concatMap (toTupleList []) $ getEdgeList vertex ring gConf
  edgeList2 = getEdgeList2 ring vertex


checkNum :: Int
checkNum = -99
-- チェック1回目
getEdgeNoSub1 :: [[Int]] -> TpEdgeNo -> TpEdgeNo
getEdgeNoSub1 []            edgeNo = edgeNo
getEdgeNoSub1 ([a, b] : xs) edgeNo = edgeNo4 where
  edgeNo4
    | edgeNo !! a !! b == 0 = debugLogStrip ("[a, b] set "    ++ show a ++ " " ++ show b ++ " ") $ getEdgeNoSub1 xs edgeNo3
    | otherwise             = debugLogStrip ("[a, b] cancel " ++ show a ++ " " ++ show b ++ " ") $ getEdgeNoSub1 xs edgeNo
  edgeNo2 = edgeNo  & (ix a <<< ix b) .~ checkNum
  edgeNo3 = edgeNo2 & (ix b <<< ix a) .~ checkNum
getEdgeNoSub1 _ _                                = error "getEdgeNoSub1 error!!"
-- チェック2回目、書き込み
getEdgeNoSub2 :: [[Int]] -> Int -> TpEdgeNo -> TpEdgeNo
getEdgeNoSub2 []            _    edgeNo = edgeNo
getEdgeNoSub2 ([a, b] : xs) term edgeNo = edgeNo4 where
  edgeNo4
    | edgeNo !! a !! b == checkNum = getEdgeNoSub2 xs (term - 1) edgeNo3
    | otherwise                    = getEdgeNoSub2 xs term       edgeNo
  edgeNo2 = edgeNo  & (ix a <<< ix b) .~ term
  edgeNo3 = edgeNo2 & (ix b <<< ix a) .~ term
getEdgeNoSub2 _ _ _                    = error "getEdgeNoSub2 error!!"
getEdgeList2 :: Int -> Int -> [[Int]]
getEdgeList2 ring vertex = [[x, y] | x <- [(ring + 2)..vertex], y <- [(ring + 1)..(x - 1)]]
  ++ [[x, y] | x <- [1..ring], y <- [(ring + 1)..vertex]]


newEdgeNo :: Int -> Int -> TpEdgeNo -> TpEdgeNo
newEdgeNo v ring edgeNo
  | v > ring  = edgeNo
  | otherwise = newEdgeNo (v + 1) ring edgeNo2 where
      u       = if v > 1 then v - 1 else ring
      edgeNo1 = edgeNo  & (ix u <<< ix v) .~ v
      edgeNo2 = edgeNo1 & (ix v <<< ix u) .~ v


getEdgeList :: Int -> Int -> TpConfFmt -> [[Int]]
getEdgeList vertex ring gConf = debugLogStrip ("$$$ edgeLists: " ++ show (concatMap (toTupleList []) (edgeList1 ++ edgeList2))) (reverse edgeList1 ++ reverse edgeList2) where
  edgeLists = splitAt ring . take vertex . drop 3 $ gConf
  edgeList1 = zipWith (++) (map (: []) [1..ring])          (map (sortOn Down . filter (> ring))        $ fst edgeLists)
  edgeList2 = zipWith (++) (map (: []) [(ring+1)..vertex]) (map (sortOn Down . filter (> ring) . tail) $ snd edgeLists)


toTupleList :: [[Int]] -> [Int] -> [[Int]]
toTupleList _   []       = []
toTupleList _   [_]      = []
toTupleList acm [x, y]   = acm ++ [[x,y]]
toTupleList acm (x:y:xs) = toTupleList (acm ++ [[x,y]]) (x:xs)



