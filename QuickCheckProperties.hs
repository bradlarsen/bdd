module QuickCheckProperties ( bddTests ) where

import BoolExpr
import BDD.Raw
import TestSuite

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
withBddMgr nVars act = do
    mgr <- bdd_mgr_create (fromIntegral nVars)
    act mgr `finally` bdd_mgr_destroy mgr

-- | Converts the given IO Boolean action into a QuickCheck property.
ioProperty :: IO Bool -> Property
ioProperty test = monadicIO (run test >>= assert)

bddProperty :: Int -> (BddMgr -> IO Bool) -> Property
bddProperty nvs = ioProperty . withBddMgr nvs

-- | Combinator to allow easier expression of properties that take a
-- Boolean expression and construct a BDD from it.
exprBddProperty
  :: (BddMgr -> BoolExpr -> Bdd -> IO Bool) -> BoolExpr -> Property
exprBddProperty prop expr = ioProperty $ withBddMgr (numVars expr) $ \mgr ->
    prop mgr expr =<< buildBdd mgr expr

-- | Constructs all possible assignments for n variables, i.e., all
-- sequences of Booleans of length n.
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments nVars = concatMap choose (assignments (pred nVars))
    where choose a = [True : a, False : a]

-- | Tests whether the given unary Boolean function is
-- equivalent to the given unary BDD function.
equiv1
    :: (Bool -> Bool)              -- ^ Boolean function
    -> (BddMgr -> Bdd -> IO Bdd)   -- ^ BDD function
    -> BoolExpr                    -- ^ expression to test on
    -> Property
equiv1 op bddOp = exprBddProperty $ \mgr expr bddExpr ->
    let cmp :: [Bool] -> IO Bool
        cmp as = do let expr' = op (eval as expr)
                    bddExpr' <- evalBdd as mgr =<< bddOp mgr bddExpr
                    return (expr' == bddExpr')
    in liftM and $ mapM cmp (assignments $ numVars expr)

-- | Tests where the given binary Boolean function is equivalent to
-- the given binary BDD function.
equiv2
  :: (Bool -> Bool -> Bool)
  -> (BddMgr -> Bdd -> Bdd -> IO Bdd)
  -> BoolExpr
  -> BoolExpr
  -> Property
equiv2 op bddOp e1 e2 =
    let nVars = max (numVars e1) (numVars e2) in
    ioProperty $ withBddMgr nVars $ \mgr ->
    let cmp :: [Bool] -> IO Bool
        cmp as = do e1Bdd <- buildBdd mgr e1
                    e2Bdd <- buildBdd mgr e2
                    let e = op (eval as e1) (eval as e2)
                    eBdd <- evalBdd as mgr =<< bddOp mgr e1Bdd e2Bdd
                    return (e == eBdd)
    in liftM and $ mapM cmp (assignments $ nVars)

-- | Test whether the binary BDD function is idempotent.
idempotent2
    :: (BddMgr -> Bdd -> Bdd -> IO Bdd)
    -> BoolExpr
    -> Property
idempotent2 op = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- op mgr bddExpr bddExpr
    return (bddExpr == bddExpr')



prop_initialNumNodes :: Int -> Property
prop_initialNumNodes nVars =
    bddProperty nVars (liftM (2 ==) . bdd_mgr_get_num_nodes)

prop_correctNumVars :: Int -> Property
prop_correctNumVars nVars =
    bddProperty nVars (liftM (fromIntegral nVars ==) . bdd_mgr_get_num_vars)

prop_selfImplies :: BoolExpr -> Property
prop_selfImplies = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- bdd_implies mgr bddExpr bddExpr
    return (bddExpr' == bdd_true)

prop_selfEquiv :: BoolExpr -> Property
prop_selfEquiv = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- bdd_equiv mgr bddExpr bddExpr
    return (bddExpr' == bdd_true)

prop_selfXOr :: BoolExpr -> Property
prop_selfXOr = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- bdd_xor mgr bddExpr bddExpr
    return (bddExpr' == bdd_false)

prop_not :: BoolExpr -> Property
prop_not = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- bdd_not mgr bddExpr
    return (bddExpr /= bddExpr')

prop_doubleNegation :: BoolExpr -> Property
prop_doubleNegation = exprBddProperty $ \mgr _expr bddExpr -> do
    bddExpr' <- bdd_not mgr =<< bdd_not mgr bddExpr
    return (bddExpr == bddExpr')

prop_satCount :: BoolExpr -> Property
prop_satCount = exprBddProperty $ \mgr expr bddExpr -> do
    let numSols = length . filter (flip eval expr) . assignments . numVars $ expr
    numSols' <- bdd_sat_count mgr bddExpr
    return (fromIntegral numSols == numSols')


bddTests :: Test
bddTests = Suite "Tests of the BDD.Raw module" [
      Suite "Manager accessor tests" [
            Test "initial number of nodes" prop_initialNumNodes
          , Test "number of variables" prop_correctNumVars
          ]
    , Suite "Truth table equivalence tests" [
            Test "NOT" (equiv1 not bdd_not)
          , Test "AND" (equiv2 (&&) bdd_and)
          , Test "OR" (equiv2 (||) bdd_or)
          , Test "NAND" (equiv2 (\x y -> not (x && y)) bdd_nand)
          , Test "IMPLIES" (equiv2 (\x y -> not x || y) bdd_implies)
          , Test "XOR" (equiv2 (/=) bdd_xor)
          , Test "EQUIV" (equiv2 (==) bdd_equiv)
          , Test "symbolic evaluation" (equiv1 id (const return))
          ]
    , Suite "Logical identity tests" [
            Test "x /\\ x === x" (idempotent2 bdd_and)
          , Test "x \\/ x === x" (idempotent2 bdd_or)
          , Test "x x === True" prop_selfImplies
          , Test "(x <=> x) === True" prop_selfEquiv
          , Test "(x XOR x) === False" prop_selfXOr
          , Test "~x =/= x" prop_not
          , Test "~~x === x" prop_doubleNegation
          ]
    , Test "sat count" prop_satCount
    ]

-- Still to test:
--   * bdd_mgr_get_ith_var
--   * bdd_mgr_get_num_nodes invariance
--   * bdd_restrict
--   * bdd_existential
--   * bdd_universal
--   * bdd_compose
--   * bdd_get_num_nodes
