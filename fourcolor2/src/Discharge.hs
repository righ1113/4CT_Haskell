{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
--{-# LANGUAGE Strict #-} -- where節 も正格評価してしまう
module Discharge ( discharge ) where

import CoLibCConst
  ( TpReducePack, TpPosout, TpGoodConf, TpCond, TpAxle, maxlev, cartvert, infty, maxoutlets, maxelist, downSym )
import DiLibApply ( chkApply )
import DiLibCaseSplit ( caseSplit1, caseSplit2 )
import DiLibDischarge2 ( dischgCore )
import Control.Lens ( (&), (.~), Ixed(ix) )
--import Data.Maybe ( fromJust, isNothing )
import Debug.Trace (trace)
type TpDisFmt = (
  (Int, TpAxle, [[String]], Int),
  (TpCond, (TpPosout, Int)),
  TpReducePack )



discharge :: (Int, String, String, String) -> Bool
discharge _ = False
discharge (deg, rulesStr, gConfsStrDi, tacStr) =
  p . until p (disReduce (read gConfsStrDi :: [TpGoodConf]) . dischg (read rulesStr :: TpPosout) . apply . caseSplit) $ makeDisData deg tacStr where
    p :: TpDisFmt -> Bool -- 真を返す時が、終了条件
    p ( (_, (_, _, -1), ["Q.E.D."] : _, _), _, _) = True
    p _                                           = False


makeDisData :: Int -> String -> TpDisFmt
makeDisData deg tacStr =
  let
    -- TpAxle
    axles0 = replicate maxlev $ replicate cartvert 0
    axLow  = take cartvert ([deg] ++ replicate (5 * deg) 5     ++ repeat 0) : axles0
    axUpp  = take cartvert ([deg] ++ replicate (5 * deg) infty ++ repeat 0) : axles0
    -- sym
    symNum = replicate (2 * maxoutlets) 0
    symNol = replicate (2 * maxoutlets) 0
    symVal = replicate (2 * maxoutlets) 0
    symPos = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    symLow = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    symUpp = replicate (2 * maxoutlets) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    -- TpReducePack
    aSLow    = replicate (maxlev + 1) $ replicate cartvert 0
    aSUpp    = replicate (maxlev + 1) $ replicate cartvert 0
    adjmat   = replicate cartvert $ replicate cartvert 0
    edgelist = replicate 12 $ replicate 9 $ replicate maxelist 0
    used     = replicate cartvert False
    image    = replicate cartvert 0
    -- tactics
    tac = tail (map words (lines tacStr))
  in
    ( (deg, (axLow, axUpp, 0), tac, 2),
      ((replicate maxlev 0, replicate maxlev 0), ((symNum, symNol, symVal, symPos, symLow, symUpp), 0)),
      ((aSLow, aSUpp, 0), used, image, adjmat, edgelist) )



caseSplit :: TpDisFmt -> TpDisFmt
caseSplit ( (deg, ax@(_, _, axLev), (_ : "C" : nStr : mStr : _) : tailTac, lineno), ((nn, mm), (sym, nosym)), r ) = trace (show nosym ++ " caseSplit. " ++ show lineno)
  ( (deg, caseSplit1 (nn, mm) ax (read nStr :: Int) (read mStr :: Int), tailTac, lineno + 1),
    (((nn & ix axLev .~ (read nStr :: Int)) & ix (axLev + 1) .~ 0, (mm & ix axLev .~ (read mStr :: Int)) & ix (axLev + 1) .~ 0), caseSplit2 (nn, mm) ax deg sym nosym lineno), r )
caseSplit dat = dat


apply :: TpDisFmt -> TpDisFmt
apply ( (deg, ax@(axLow, axUpp, axLev), (_ : "S" : nowTac) : tailTac, lineno), (nm, (sym@(_, symNol, _, _, _, _), nosym)), r ) =
  trace (show nosym ++ " apply. " ++ show lineno)
    ( (chkApply nowTac ax sym nosym deg, (axLow, axUpp, axLev - 1), tailTac, lineno + 1), (nm, (sym, downSym nosym symNol axLev)), r )
apply dat = dat


dischg :: TpPosout -> TpDisFmt -> TpDisFmt
dischg rules ( (deg, ax@(axLow, axUpp, axLev), (_ : "H" : nowTac) : tailTac, lineno), (nm, (sym@(_, symNol, _, _, _, _), nosym)), reducePack ) =
  trace (show nosym ++ " discharge. " ++ show lineno)
    ( (dischgCore nowTac ax rules deg reducePack, (axLow, axUpp, axLev - 1), tailTac, lineno + 1), (nm, (sym, downSym nosym symNol axLev)), reducePack )
dischg _ dat = dat


disReduce :: [TpGoodConf] -> TpDisFmt -> TpDisFmt
disReduce gConfs ( (deg, (axLow, axUpp, axLev), (_ : "R" : _) : tailTac, lineno), (nm, (sym@(_, symNol, _, _, _, _), nosym)), ((aSLow, aSUpp, aSLev), used, image, adjmat, elist) ) =
  trace (show nosym ++ " reduce. " ++ show lineno)
    ( (deg, (axLow, axUpp, axLev - 1), tailTac, lineno + 1), (nm, (sym, downSym nosym symNol axLev)),
      disReduceCore gConfs ( (aSLow & ix 0 .~ axLow !! axLev, aSUpp & ix 0 .~ axUpp !! axLev, aSLev), used, image, adjmat, elist ) )
disReduce _ dat = dat --error "Invalid instruction"
disReduceCore :: [TpGoodConf] -> TpReducePack -> TpReducePack
disReduceCore _ rp = rp



