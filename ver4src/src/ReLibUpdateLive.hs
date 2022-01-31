module ReLibUpdateLive where

import CoLibCConst                    ( TpLiveTwin, TpUpdateState, simatchnumber, maxring )
import Control.Lens                   ( (&), (.~), Ixed(ix) )
import Control.Monad.Trans.State.Lazy ( StateT(..), execStateT )
import Data.Bits                      ( Bits(shift, (.&.), (.|.)) )    
import Data.Function                  ( fix )
import Data.Int                       ( Int8 )


updateLive :: Int -> Int -> Int -> TpLiveTwin -> IO TpLiveTwin
updateLive ring nchar ncodes lTwin =
  flip fix (lTwin, real, 0, 1, 0) $ \loop now -> do
    next@((nlive, live), _, _, _, _) <- execStateT testmatch now
    is                               <- isUpdate
    case () of
      _ | not is    -> return (nlive, live)
        | otherwise -> loop next where
            c2 = 1
  where real = replicate (simatchnumber !! maxring `div` 8 + 2) 255


isUpdate = return False


testmatch :: StateT TpUpdateState IO ()
testmatch = return ()



