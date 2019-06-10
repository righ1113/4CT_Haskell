module Lib
    ( myLoop
    ) where

-- ユーティリティ：蓄積変数付き、脱出可能なloop関数
-- 参考記事：https://mkotha.hatenadiary.org/entry/20110430/1304122048
myLoop :: (accT -> a -> (accT -> b) -> b) -> (accT -> b) -> accT -> [a] -> b
myLoop _ g acc []     = g acc
myLoop f g acc (x:xs) = f acc x $ \acc' -> myLoop f g acc' xs



