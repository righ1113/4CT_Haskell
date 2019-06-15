module Lib
    ( TpConfmat,
      readFileUnavoidR,
      myLoop,
      foldlCont
    ) where

import Control.Monad.Trans.Cont (Cont)
import Data.Foldable            (foldlM)

type TpConfmat = [[Int]]


readFileUnavoidR :: IO [TpConfmat]
readFileUnavoidR = do
  unaStr <- readFile "readFile/unavoidable2HS.txt"
  return $ map tail (read unaStr :: [TpConfmat])

-- ユーティリティ：蓄積変数付き、脱出可能なloop関数
-- 参考記事：https://mkotha.hatenadiary.org/entry/20110430/1304122048
myLoop :: (accT -> a -> (accT -> b) -> b) -> (accT -> b) -> accT -> [a] -> b
myLoop _ g acc []     = g acc
myLoop f g acc (x:xs) = f acc x $ \acc' -> myLoop f g acc' xs

-- 同じもの
foldlCont :: (b -> a -> Cont r b) -> b -> [a] -> Cont r b
foldlCont = foldlM


