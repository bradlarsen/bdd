module QuickCheckProperties where

import BoolExpr
import qualified BDD.Raw as Raw

import Control.Exception (finally)
import Control.Monad (liftM)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.QuickCheck (Property)

-- | Brackets an action that requires a BDD manager with manager
-- creation and destruction.
withBddMgr
  :: Int                  -- ^ the number of variables
  -> (Raw.BddMgr -> IO a) -- ^ the action to run
  -> IO a
withBddMgr numVars act = do
    mgr <- Raw.bdd_mgr_create (fromIntegral numVars)
    act mgr `finally` Raw.bdd_mgr_destroy mgr

-- | Converts the given IO Boolean action into a QuickCheck property.
ioProperty :: IO Bool -> Property
ioProperty test = monadicIO (run test >>= assert)

bddProp :: (Raw.BddMgr -> BoolExpr -> IO Bool) -> AnnBoolExpr -> Property
bddProp prop (Ann (numVars, expr)) =
    ioProperty $ withBddMgr numVars $ flip prop expr

bddProp' :: (Raw.BddMgr -> Int -> BoolExpr -> IO Bool) -> AnnBoolExpr -> Property
bddProp' prop (Ann (numVars, expr)) =
    ioProperty $ withBddMgr numVars $ \mgr -> prop mgr numVars expr


-- | Constructs all possible assignments for n variables, i.e., all
-- sequences of Booleans of length n.
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments numVars = concatMap choose (assignments (pred numVars))
    where choose a = [True : a, False : a]


-- Compares the symbolical BDD interpretation of the given Boolean
-- expression with a truth table.
prop_bddSymbolicEvalOk :: AnnBoolExpr -> Property
prop_bddSymbolicEvalOk = bddProp' $ \mgr numVars expr -> do
    bdd <- buildBdd mgr expr
    let cmp :: [Bool] -> IO Bool
        cmp as = liftM (eval expr as ==) (evalBdd mgr bdd as)
    liftM and $ mapM cmp (assignments numVars)

prop_initialNumNodes :: Int -> Property
prop_initialNumNodes numVars = ioProperty $ withBddMgr numVars $ \mgr -> do
    liftM (2 ==) (Raw.bdd_mgr_get_num_nodes mgr)

prop_correctNumVars :: Int -> Property
prop_correctNumVars numVars = ioProperty $ withBddMgr numVars $ \mgr -> do
    liftM (fromIntegral numVars ==) (Raw.bdd_mgr_get_num_vars mgr)

testIdempotent
  :: (Raw.BddMgr -> Raw.Bdd -> Raw.Bdd -> IO Raw.Bdd)
  -> Raw.BddMgr
  -> BoolExpr
  -> IO Bool
testIdempotent op mgr expr = do
    bdd <- buildBdd mgr expr
    bdd' <- op mgr bdd bdd
    return (bdd == bdd')

prop_andIdempotent :: AnnBoolExpr -> Property
prop_andIdempotent = bddProp $ testIdempotent Raw.bdd_and

prop_orIdempotent :: AnnBoolExpr -> Property
prop_orIdempotent = bddProp $ testIdempotent Raw.bdd_or

prop_selfImplies :: AnnBoolExpr -> Property
prop_selfImplies = bddProp $ \mgr expr -> do
    bdd <- buildBdd mgr expr
    bdd' <- Raw.bdd_implies mgr bdd bdd
    return (bdd' == Raw.bdd_true)

prop_selfEquiv :: AnnBoolExpr -> Property
prop_selfEquiv = bddProp $ \mgr expr -> do
    bdd <- buildBdd mgr expr
    bdd' <- Raw.bdd_equiv mgr bdd bdd
    return (bdd' == Raw.bdd_true)

prop_selfXOr :: AnnBoolExpr -> Property
prop_selfXOr = bddProp $ \mgr expr -> do
    bdd <- buildBdd mgr expr
    bdd' <- Raw.bdd_xor mgr bdd bdd
    return (bdd' == Raw.bdd_false)

prop_not :: AnnBoolExpr -> Property
prop_not = bddProp $ \mgr expr -> do
    bdd <- buildBdd mgr expr
    bdd' <- Raw.bdd_not mgr bdd
    return (bdd /= bdd')

prop_doubleNegation :: AnnBoolExpr -> Property
prop_doubleNegation = bddProp $ \mgr expr -> do
    bdd <- buildBdd mgr expr
    bdd' <- Raw.bdd_not mgr =<< Raw.bdd_not mgr bdd
    return (bdd == bdd')

prop_satCount :: AnnBoolExpr -> Property
prop_satCount = bddProp' $ \mgr numVars expr -> do
    let numSols = length . filter (eval expr) . assignments $ numVars
    bdd <- buildBdd mgr expr
    numSols' <- Raw.bdd_sat_count mgr bdd
    return (fromIntegral numSols == numSols')
