module ReLibFindlive where

import CoLibCConst   ( TpAngle )


findlive :: [Int] -> Int -> TpAngle -> [Int] -> Int -> IO (Int, [Int])
findlive live0 ncodes angle power extentclaim
  = return (1, live0)



