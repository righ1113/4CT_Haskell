module ReLibStrip where

import CoLibCConst   ( edges, debugLogStrip, TpConfmat, TpEdgeNo )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.List     ( sortOn )
import Data.Ord      ( Down(..) )


getEdgeNo :: Int -> Int -> TpConfmat -> TpEdgeNo
getEdgeNo vertex ring gConf = debugLogStrip ("$$$ vertex: " ++ show edgeNo) edgeNo where
  edgeNo   = getEdgeNoSub edgeList (ring + 1) edgeNo0
  edgeNo0  = newEdgeNo 1 ring (replicate edges $ replicate edges 0)
  edgeList = concatMap (toTupleList []) $ getEdgeList vertex ring gConf


getEdgeNoSub :: [[Int]] -> Int -> TpEdgeNo -> TpEdgeNo
getEdgeNoSub []            _    edgeNo = edgeNo
getEdgeNoSub ([a, b] : xs) term edgeNo = edgeNo4 where
  edgeNo4
    | edgeNo !! a !! b == 0 = getEdgeNoSub xs (term + 1) edgeNo3
    | otherwise             = getEdgeNoSub xs term edgeNo
  edgeNo2 = edgeNo  & (ix a <<< ix b) .~ term
  edgeNo3 = edgeNo2 & (ix b <<< ix a) .~ term


newEdgeNo :: Int -> Int -> TpEdgeNo -> TpEdgeNo
newEdgeNo v ring edgeNo
  | v > ring  = edgeNo
  | otherwise = newEdgeNo (v + 1) ring edgeNo2 where
      u       = if v > 1 then v - 1 else ring
      edgeNo1 = edgeNo  & (ix u <<< ix v) .~ v
      edgeNo2 = edgeNo1 & (ix v <<< ix u) .~ v


getEdgeList :: Int -> Int -> TpConfmat -> [[Int]]
getEdgeList vertex ring gConf = debugLogStrip ("$$$ edgeLists: " ++ show (concatMap (toTupleList []) (edgeList1 ++ edgeList2))) (reverse edgeList1 ++ reverse edgeList2) where
  edgeLists = splitAt ring . take vertex . drop 3 $ gConf
  edgeList1 = zipWith (++) (map (: []) [1..ring])          (map (sortOn Down . filter (> ring))        $ fst edgeLists)
  edgeList2 = zipWith (++) (map (: []) [(ring+1)..vertex]) (map (sortOn Down . filter (> ring) . tail) $ snd edgeLists)


toTupleList :: [[Int]] -> [Int] -> [[Int]]
toTupleList _   []       = []
toTupleList _   [x]      = []
toTupleList acm [x, y]   = acm ++ [[x,y]] 
toTupleList acm (x:y:xs) = toTupleList (acm ++ [[x,y]]) (x:xs)



