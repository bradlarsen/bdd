{-# LANGUAGE ForeignFunctionInterface,
             GeneralizedNewtypeDeriving
             #-}
module BDD.Raw where

import Foreign (Ptr)
import Foreign.C.Types (CInt, CUInt, CDouble)

#include "bddlib.h"

newtype CBoolean = CBoolean CInt
    deriving (Eq, Ord, Show)

ctrue :: CBoolean
ctrue = CBoolean 1

cfalse :: CBoolean
cfalse = CBoolean 0

toCBoolean :: Bool -> CBoolean
toCBoolean True = ctrue
toCBoolean False = cfalse

-- * BDD manager type and operations

newtype BddMgr = BddMgr (Ptr BddMgr)

foreign import ccall "bddlib.h bdd_mgr_create"
    bdd_mgr_create :: CUInt -> IO BddMgr

foreign import ccall "bddlib.h bdd_mgr_create_with_hint"
    bdd_mgr_create_with_hint :: CUInt -> CUInt -> IO BddMgr

foreign import ccall "bddlib.h bdd_mgr_destroy"
    bdd_mgr_destroy :: BddMgr -> IO ()

foreign import ccall "bddlib.h bdd_mgr_get_num_vars"
    bdd_mgr_get_num_vars :: BddMgr -> IO CUInt

foreign import ccall "bddlib.h bdd_mgr_get_num_nodes"
    bdd_mgr_get_num_nodes :: BddMgr -> IO CUInt

foreign import ccall "bddlib.h bdd_mgr_get_num_allocated"
    bdd_mgr_get_num_allocated :: BddMgr -> IO CUInt

foreign import ccall "bddlib.h bdd_mgr_perform_gc"
    bdd_mgr_perform_gc :: BddMgr -> IO ()

-- * BDD type, constants, and accessors

newtype Bdd = Bdd (Ptr Bdd)
    deriving (Eq, Ord, Show)

foreign import ccall "bddlib.h bdd_var"
    bdd_var :: BddMgr -> Bdd -> IO CUInt

foreign import ccall "bddlib.h bdd_low"
    bdd_low :: BddMgr -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_high"
    bdd_high :: BddMgr -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_false"
    bdd_false :: BddMgr -> IO Bdd

foreign import ccall "bddlib.h bdd_true"
    bdd_true :: BddMgr -> IO Bdd

-- * BDD reference counting operations

foreign import ccall "bddlib.h bdd_inc_ref"
    bdd_inc_ref :: BddMgr -> Bdd -> IO ()

foreign import ccall "bddlib.h bdd_dec_ref"
    bdd_dec_ref :: BddMgr -> Bdd -> IO ()

-- * BDD operations

foreign import ccall "bddlib.h bdd_ith_var"
    bdd_ith_var :: BddMgr -> CUInt -> IO Bdd

foreign import ccall "bddlib.h bdd_and"
    bdd_and :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_or"
    bdd_or :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_xor"
    bdd_xor :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_equiv"
    bdd_equiv :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_nand"
    bdd_nand :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_implies"
    bdd_implies :: BddMgr -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_not"
    bdd_not :: BddMgr -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_ite"
    bdd_ite :: BddMgr -> Bdd -> Bdd -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_restrict"
    bdd_restrict :: BddMgr -> Bdd -> CUInt -> CBoolean -> IO Bdd

foreign import ccall "bddlib.h bdd_existential"
    bdd_existential :: BddMgr -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_universal"
    bdd_universal :: BddMgr -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_compose"
    bdd_compose :: BddMgr -> Bdd -> CUInt -> Bdd -> IO Bdd

foreign import ccall "bddlib.h bdd_sat_count"
    bdd_sat_count :: BddMgr -> Bdd -> IO CDouble

foreign import ccall "bddlib.h bdd_get_num_nodes"
    bdd_get_num_nodes :: BddMgr -> Bdd -> IO CUInt
