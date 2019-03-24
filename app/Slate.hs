{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}
module Slate where

import ParseJSON()
import Model
import Inference
import Util

import Control.Monad.Writer()
import Control.Monad.State

import qualified Data.Map as M
import Data.List.Split
import Data.Function

import Debug.Trace

import qualified Data.Graph.Inductive as G


getVarTypes :: String -> [(Var, Type)]
getVarTypes s =
  s & filter (/= ' ') & lines & map (splitOn ":") &
  map (\[a, b] -> (CVar a, read b)) --HACK non-exhaustive pattern match!!!

f = readFile "b_controller.types"
vs = [(CVar "tmpB",Low),(CVar "ea",High)]

t@(CTransition src dst gua r) = CTransition (CMode "ModeB4" (CFlow [CAssignment (CVar "zb_dot") (CExpr "1"),CAssignment (CVar "Va_dot") (CExpr "fa"),CAssignment (CVar "Va_out") (CExpr "Va"),CAssignment (CVar "fa_dot") (CExpr "0"),CAssignment (CVar "fa_out") (CExpr "fa"),CAssignment (CVar "Vb_dot") (CExpr "fb"),CAssignment (CVar "Vb_out") (CExpr "Vb"),CAssignment (CVar "fb_dot") (CExpr "0"),CAssignment (CVar "fb_out") (CExpr "fb"),CAssignment (CVar "Vc_dot") (CExpr "fc"),CAssignment (CVar "Vc_out") (CExpr "Vc"),CAssignment (CVar "fc_dot") (CExpr "0"),CAssignment (CVar "fc_out") (CExpr "fc")])) (CMode "ModeB1" (CFlow [CAssignment (CVar "Va_dot") (CExpr "fa"),CAssignment (CVar "Va_out") (CExpr "Va"),CAssignment (CVar "fa_dot") (CExpr "0"),CAssignment (CVar "fa_out") (CExpr "fa"),CAssignment (CVar "Vb_dot") (CExpr "fb"),CAssignment (CVar "Vb_out") (CExpr "Vb"),CAssignment (CVar "fb_dot") (CExpr "0"),CAssignment (CVar "fb_out") (CExpr "fb"),CAssignment (CVar "Vc_dot") (CExpr "fc"),CAssignment (CVar "Vc_out") (CExpr "Vc"),CAssignment (CVar "fc_dot") (CExpr "0"),CAssignment (CVar "fc_out") (CExpr "fc")])) (CGuard (CExpr "zb>=Tcb&&eb+ea>0")) (CReset [CAssignment (CVar "fb") (CExpr "fb+tmpB"),CAssignment (CVar "zb") (CExpr "0")])

t_st = gencs t
src_st = gencs src

gencs :: (Sing l) => Component l -> CGenState
gencs l = execState (genConstraints l) emptyState

emptyM = CMode "empty" (CFlow [])

t2 = CTransition emptyM emptyM gua r
t2_st = gencs t2

asn = CAssignment (CVar "fb") (CExpr "fb+tmpB")



m =
  CModel
    [ CMode "ModeB1" (CFlow [CAssignment (CVar "Va_dot") (CExpr "ea")])
    , CMode "ModeB3" (CFlow [CAssignment (CVar "zb_dot") (CExpr "1")])
    , CMode "ModeB5" (CFlow [CAssignment (CVar "zb_dot") (CExpr "1")])
    ]
    [ CTransition
        (CMode "ModeB1" (CFlow [CAssignment (CVar "Va_dot") (CExpr "fa")]))
        (CMode "ModeB3" (CFlow [CAssignment (CVar "zb_dot") (CExpr "1")]))
        (CGuard (CExpr "eb>=0"))
        (CReset
           [ CAssignment (CVar "tmpB") (CExpr "eb")
           , CAssignment (CVar "zb") (CExpr "0")
           ])
    ]

t' = CTransition
        (CMode "ModeB1" (CFlow [CAssignment (CVar "Va_dot") (CExpr "fa")]))
        (CMode "ModeB3" (CFlow [CAssignment (CVar "zb_dot") (CExpr "1")]))
        (CGuard (CExpr "eb>=0"))
        (CReset
           [ CAssignment (CVar "tmpB") (CExpr "eb")
           , CAssignment (CVar "zb") (CExpr "0")
           ])

m_st@CGenState{cmap = cmap', nmap = nmap', constraints = cs'} = gencs m

g = buildDepGraph nmap' cmap' cs'
result = checkDepGraph g


ty = trace "hi" $ typeIn t' m vs
