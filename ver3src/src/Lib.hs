module Lib
    ( TpConfmat,
      readFileGoodConfsR,
      myLoop,
      foldlCont
    ) where

import Control.Monad.Trans.Cont (Cont, ContT)
import Data.Foldable            (foldlM)

type TpConfmat = [[Int]]


readFileGoodConfsR :: IO [TpConfmat]
readFileGoodConfsR = do
  unaStr <- readFile "readFile/goodConfs.txt"
  return $ map tail (read unaStr :: [TpConfmat])

-- ユーティリティ：蓄積変数付き、脱出可能なloop関数
-- 参考記事：https://mkotha.hatenadiary.org/entry/20110430/1304122048
myLoop :: (accT -> a -> (accT -> b) -> b) -> (accT -> b) -> accT -> [a] -> b
myLoop _ g acc []     = g acc
myLoop f g acc (x:xs) = f acc x $ \acc' -> myLoop f g acc' xs

-- 同じもの
foldlCont :: (b -> a -> ContT r m b) -> b -> [a] -> ContT r m b
foldlCont = foldlM



