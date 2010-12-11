module QuickCheckProperties where

import BoolExpr
import BDD.Raw

import Control.Exception (finally)
import Control.Monad (liftM)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.QuickCheck (Property)

-- | Brackets an action that requires a BDD manager with manager
-- creation and destruction.
withBddMgr
  :: Int                  -- ^ the number of variables
  -> (BddMgr -> IO a)     -- ^ the action to run
  -> IO a
withBddMgr numVars act = do
    mgr <- bdd_mgr_create (fromIntegral numVars)
    act mgr `finally` bdd_mgr_destroy mgr

-- | Converts the given IO Boolean action into a QuickCheck property.
ioProperty :: IO Bool -> Property
ioProperty test = monadicIO (run test >>= assert)

bddProperty :: Int -> (BddMgr -> IO Bool) -> Property
bddProperty numVars = ioProperty . withBddMgr numVars


-- | Constructs all possible assignments for n variables, i.e., all
-- sequences of Booleans of length n.
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments numVars = concatMap choose (assignments (pred numVars))
    where choose a = [True : a, False : a]


-- | Compares the symbolical BDD interpretation of the given Boolean
-- expression with a truth table.
prop_bddSymbolicEvalOk :: BoolExpr -> Property
prop_bddSymbolicEvalOk expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    let cmp :: [Bool] -> IO Bool
        cmp as = liftM (eval expr as ==) (evalBdd mgr bdd as)
    liftM and $ mapM cmp (assignments $ numVars expr)

prop_initialNumNodes :: Int -> Property
prop_initialNumNodes numVars =
    bddProperty numVars (liftM (2 ==) . bdd_mgr_get_num_nodes)

prop_correctNumVars :: Int -> Property
prop_correctNumVars numVars =
    bddProperty numVars (liftM (fromIntegral numVars ==) . bdd_mgr_get_num_vars)

idempotent2
    :: (BddMgr -> Bdd -> Bdd -> IO Bdd)
    -> BoolExpr
    -> Property
idempotent2 op expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- op mgr bdd bdd
    return (bdd == bdd')

prop_andIdempotent :: BoolExpr -> Property
prop_andIdempotent = idempotent2 bdd_and

prop_orIdempotent :: BoolExpr -> Property
prop_orIdempotent = idempotent2 bdd_or

prop_selfImplies :: BoolExpr -> Property
prop_selfImplies expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- bdd_implies mgr bdd bdd
    return (bdd' == bdd_true)

prop_selfEquiv :: BoolExpr -> Property
prop_selfEquiv expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- bdd_equiv mgr bdd bdd
    return (bdd' == bdd_true)

prop_selfXOr :: BoolExpr -> Property
prop_selfXOr expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- bdd_xor mgr bdd bdd
    return (bdd' == bdd_false)

prop_not :: BoolExpr -> Property
prop_not expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- bdd_not mgr bdd
    return (bdd /= bdd')

prop_doubleNegation :: BoolExpr -> Property
prop_doubleNegation expr = bddProperty (numVars expr) $ \mgr -> do
    bdd <- buildBdd mgr expr
    bdd' <- bdd_not mgr =<< bdd_not mgr bdd
    return (bdd == bdd')

prop_satCount :: BoolExpr -> Property
prop_satCount expr = bddProperty (numVars expr) $ \mgr -> do
    let numSols = length . filter (eval expr) . assignments . numVars $ expr
    bdd <- buildBdd mgr expr
    numSols' <- bdd_sat_count mgr bdd
    return (fromIntegral numSols == numSols')
