{-# LANGUAGE ForeignFunctionInterface,
             GeneralizedNewtypeDeriving
             #-}
module BDD.Raw where

import Foreign (Ptr)
import Foreign.C.Types (CUInt, CInt, CDouble)

#include "bdd.h"

newtype BddMgr = BddMgr (Ptr BddMgr)

newtype Bdd = Bdd CUInt
    deriving (Eq, Ord, Show)

bdd_true :: Bdd
bdd_true = Bdd 1

bdd_false :: Bdd
bdd_false = Bdd 0

foreign import ccall "bdd.h bdd_mgr_create"
    bdd_mgr_create :: CUInt -> IO BddMgr

foreign import ccall "bdd.h bdd_mgr_destroy"
    bdd_mgr_destroy :: BddMgr -> IO ()

foreign import ccall "bdd.h bdd_mgr_get_num_vars"
    bdd_mgr_get_num_vars :: BddMgr -> IO CUInt

foreign import ccall "bdd.h bdd_mgr_get_num_nodes"
    bdd_mgr_get_num_nodes :: BddMgr -> IO CUInt

foreign import ccall "bdd.h bdd_mgr_get_ith_var"
    bdd_mgr_get_ith_var :: BddMgr -> CUInt -> IO Bdd

foreign import ccall "bdd.h bdd_mgr_get_nith_var"
    bdd_mgr_get_nith_var :: BddMgr -> CUInt -> IO Bdd

newtype BddApplyBinop = BddApplyBinop CInt
    deriving (Eq, Ord)

#{enum BddApplyBinop, BddApplyBinop
 , bdd_binop_and     = BDD_AND
 , bdd_binop_or      = BDD_OR
 , bdd_binop_xor     = BDD_XOR
 , bdd_binop_equiv   = BDD_EQUIV
 , bdd_binop_nand    = BDD_NAND
 , bdd_binop_implies = BDD_IMPLIES
 }

foreign import ccall "bdd.h bdd_apply"
    bdd_apply :: BddMgr -> BddApplyBinop -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_and"
    bdd_and :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_or"
    bdd_or :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_xor"
    bdd_xor :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_equiv"
    bdd_equiv :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_nand"
    bdd_nand :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_implies"
    bdd_implies :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_restrict"
    bdd_restrict :: BddMgr -> Bdd -> CUInt -> Bool -> IO Bdd

foreign import ccall "bdd.h bdd_not"
    bdd_not :: BddMgr -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_existential"
    bdd_existential :: BddMgr -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_universal"
    bdd_universal :: BddMgr -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_compose"
    bdd_compose :: BddMgr -> Bdd -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bdd.h bdd_sat_count"
    bdd_sat_count :: BddMgr -> Bdd -> IO CDouble
