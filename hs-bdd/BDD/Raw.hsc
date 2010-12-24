{-# LANGUAGE ForeignFunctionInterface,
             GeneralizedNewtypeDeriving
             #-}
module BDD.Raw where

import Foreign (Ptr)
import Foreign.C.Types (CUInt, CDouble)

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

foreign import ccall "bdd.h bdd_mgr_create_with_hint"
    bdd_mgr_create_with_hint :: CUInt -> CUInt -> IO BddMgr

foreign import ccall "bdd.h bdd_mgr_destroy"
    bdd_mgr_destroy :: BddMgr -> IO ()

foreign import ccall "bdd.h bdd_mgr_get_num_vars"
    bdd_mgr_get_num_vars :: BddMgr -> IO CUInt

foreign import ccall "bdd.h bdd_mgr_get_num_nodes"
    bdd_mgr_get_num_nodes :: BddMgr -> IO CUInt

foreign import ccall "bdd.h bdd_ith_var"
    bdd_ith_var :: BddMgr -> CUInt -> IO Bdd

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

foreign import ccall "bdd.h bdd_get_num_nodes"
    bdd_get_num_nodes :: BddMgr -> Bdd -> IO CUInt
