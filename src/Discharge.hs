module Discharge where

import Prelude hiding (log)
import Control.Monad.State

--    型       値コンストラクタ            フィールド名
data Dstate = Dstate { log :: [String], hypo :: [String], goal :: [String] }

evals :: [String] -> StateT Dstate IO ()
evals []       = return ()
evals (s : ss) = do
  (lift . putStrLn) s
  Dstate dLog dHypo dGoal <- get
  put $ Dstate (dLog ++ [s]) dHypo dGoal
  let ws = words s
  evals ss

main :: IO ()
main = do
  inStr <- readFile "present5hs"
  evalStateT (evals $ lines inStr) $ Dstate [] [] []
  -- mapM_ putStrLn $ log outStr





