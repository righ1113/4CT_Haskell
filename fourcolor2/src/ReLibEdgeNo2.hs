module ReLibEdgeNo2( getEdgeNo ) where

import CoLibCConst
    ( TpEdgeNo, TpConfFmt, debugLogStrip, edgesM )
import Control.Arrow ( (<<<) )
import Control.Lens  ( (&), (.~), Ixed(ix) )
import Data.List     ( sortOn )
import Data.Ord      ( Down(..) )


getEdgeNo :: Int -> Int -> Int -> TpConfFmt -> TpEdgeNo
getEdgeNo vertex ring _term gConf = debugLogStrip ("$$$ vertex: " ++ show edgeNo) edgeNo where
  gConfL   = last gConf
  edgeNo
    | null gConfL = getEdgeNoSub edgeList [(ring + 1)..] edgeNo0 -- D reducible
    | otherwise   = getEdgeNoSub edgeList gConfL         edgeNo0 -- C reducible
  edgeNo0  = newEdgeNo 1 ring (replicate edgesM $ replicate edgesM 0)
  edgeList = concatMap (toTupleList []) $ getEdgeList vertex ring gConf


getEdgeNoSub :: [[Int]] -> [Int] -> TpEdgeNo -> TpEdgeNo
getEdgeNoSub []            _               edgeNo = edgeNo
getEdgeNoSub ([a, b] : xs) ts@(term : tss) edgeNo = edgeNo4 where
  edgeNo4
    | edgeNo !! a !! b == 0 = debugLogStrip ("[a, b] set "    ++ show a ++ " " ++ show b ++ " " ++ show term) $ getEdgeNoSub xs tss edgeNo3
    | otherwise             = debugLogStrip ("[a, b] cancel " ++ show a ++ " " ++ show b ++ " " ++ show term) $ getEdgeNoSub xs ts  edgeNo
  edgeNo2 = edgeNo  & (ix a <<< ix b) .~ term
  edgeNo3 = edgeNo2 & (ix b <<< ix a) .~ term
getEdgeNoSub _ _ _                                = error "getEdgeNoSub error!!"


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



