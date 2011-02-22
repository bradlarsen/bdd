{-# LANGUAGE Rank2Types #-}
module BDD
  (
    Mgr
  , newMgrWithHint
  , numVars
  , nodesUsed
  , nodesUsedAtLevel
  , nodesAllocated
  , Bdd
  , bddIthVar
  , bddNot
  ) where

import Control.Exception (finally)
import Control.Monad (liftM, when)
import Foreign (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr, mallocForeignPtr, poke, addForeignPtrFinalizerEnv)
import Foreign.C.Types (CUInt)
import Debug.Trace

import qualified BDD.Raw as Raw

newtype Mgr s = Mgr { unMgr :: Raw.BddMgr }
    deriving (Eq, Ord)

newtype Bdd s = Bdd { unBdd :: Raw.Bdd }
    deriving (Eq, Ord, Show)

wrapBdd :: Mgr s -> Raw.Bdd -> (Bdd s -> IO a) -> IO a
wrapBdd mgr bdd k = do
    Raw.bdd_inc_ref (unMgr mgr) bdd
    k (Bdd bdd) `finally` Raw.bdd_dec_ref (unMgr mgr) bdd

newMgrWithHint
    :: Int                        -- ^ the number of Boolean variables
    -> Int                        -- ^ suggested initial number of nodes
    -> (forall s. Mgr s -> IO a)  -- ^ the action to run
    -> IO a
newMgrWithHint nVars _nNodes _act
    | nVars <= 0 = error ("BDD.newMgrWithHint: bad number of variables '" ++
                          show nVars ++ "'")
newMgrWithHint nVars nNodes act = do
    mgr <- Raw.bdd_mgr_create_with_hint (fromIntegral nVars)
                                        (fromIntegral nNodes)
    act (Mgr mgr) `finally` Raw.bdd_mgr_destroy mgr

numVars :: Mgr s -> IO Int
numVars = liftM fromIntegral . Raw.bdd_mgr_get_num_vars . unMgr

nodesUsed :: Mgr s -> IO Int
nodesUsed = liftM fromIntegral . Raw.bdd_mgr_get_num_nodes . unMgr

nodesUsedAtLevel :: Mgr s -> Int -> IO Int
nodesUsedAtLevel mgr lvl = do
    nLevels <- numVars mgr
    when (lvl < 0 || lvl >= nLevels) $ do
        error ("BDD.nodesUsedAtLevel: invalid level '" ++ show lvl ++ "'")
    liftM fromIntegral $ Raw.bdd_mgr_get_num_nodes_at_level (unMgr mgr)
                                                            (fromIntegral lvl)

nodesAllocated :: Mgr s -> IO Int
nodesAllocated = liftM fromIntegral . Raw.bdd_mgr_get_num_allocated . unMgr

bddIthVar :: Mgr s -> Int -> (forall s. Bdd s -> IO a) -> IO a
bddIthVar mgr i k = do
    bdd <- Raw.bdd_ith_var (unMgr mgr) (fromIntegral i)
    wrapBdd mgr bdd k

bddNot :: Mgr s -> Bdd s -> (forall s. Bdd s -> IO a) -> IO a
bddNot mgr bdd k = do
    bdd' <- Raw.bdd_not (unMgr mgr) (unBdd bdd)
    wrapBdd mgr bdd' k
