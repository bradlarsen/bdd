module BoolExpr where

import BDD.Raw

import Control.Monad (liftM, liftM2, foldM)
import Test.QuickCheck
    (Gen, Arbitrary (arbitrary, shrink),
     shrinkIntegral,
     choose, sized, oneof, frequency, elements)
import Data.Array.IArray (Array, (!), listArray)

-- | Boolean expressions.
data BoolExpr
    = BFalse
    | BTrue
    | Var Int  -- ^ index is non-negative
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    | Xor BoolExpr BoolExpr
    | Equiv BoolExpr BoolExpr
    | Nand BoolExpr BoolExpr
    | Implies BoolExpr BoolExpr
    deriving (Eq, Show)

-- | Returns the number of variables in the Boolean expression, which
-- is defined as the maximum variable index in the expression plus one.
numVars :: BoolExpr -> Int
numVars expr =
    case expr of
        BFalse -> 0
        BTrue -> 0
        Var i -> i + 1
        Not e -> numVars e
        And l r -> max (numVars l) (numVars r)
        Or l r -> max (numVars l) (numVars r)
        Xor l r -> max (numVars l) (numVars r)
        Equiv l r -> max (numVars l) (numVars r)
        Nand l r -> max (numVars l) (numVars r)
        Implies l r -> max (numVars l) (numVars r)

arbitraryBoolExpr :: Int -> Int -> Gen BoolExpr
arbitraryBoolExpr nVars = go
    where
        go maxNodes = if maxNodes <= 1 then term else nonterm
            where
                term = if nVars == 0
                       then elements [BFalse, BTrue]
                       else frequency [ (1, return BFalse)
                                      , (1, return BTrue)
                                      , (12, liftM Var (choose (0, nVars - 1)))
                                      ]
                nonterm = oneof (liftM Not (go (maxNodes - 1)) : map bin binCs)
                bin cons = do s1 <- choose (1, maxNodes - 1)
                              let s2 = maxNodes - s1
                              liftM2 cons (go s1) (go s2)
                binCs = [And, Or, Xor, Equiv, Nand, Implies]

shrinkBoolExpr :: BoolExpr -> [BoolExpr]
shrinkBoolExpr expr =
    case expr of 
        BFalse -> []
        BTrue -> []
        Var i -> map Var (shrinkIntegral i)
        Not e -> [e]
        And l r -> [l, r]
        Or l r -> [l, r]
        Xor l r -> [l, r]
        Equiv l r -> [l, r]
        Nand l r -> [l, r]
        Implies l r -> [l, r]

instance Arbitrary BoolExpr where
    arbitrary = (sized . arbitraryBoolExpr) =<< choose (0, 7)
    shrink = shrinkBoolExpr

-- | Constructs all possible assignments for n variables, i.e., all
-- sequences of Booleans of length n.
assignments :: Int -> [[Bool]]
assignments 0 = [[]]
assignments nVars = concatMap assign (assignments (pred nVars))
    where assign a = [True : a, False : a]

-- | Evaluates the given Boolean expression under the given
-- environment.  The environment is a list of Boolean values, one for
-- each variable index, in order.
eval :: [Bool] -> BoolExpr -> Bool
eval varAssigns expression = go expression
    where
        env :: Array Int Bool
        env = listArray (0, length varAssigns - 1) varAssigns
        go expr = case expr of
                      BFalse -> False
                      BTrue -> True
                      Var i -> env ! i
                      Not e -> not (go e)
                      And l r -> go l && go r
                      Or l r -> go l || go r
                      Xor l r -> go l /= go r
                      Equiv l r -> go l == go r
                      Nand l r -> not (go l && go r)
                      Implies l r -> not (go l) || go r

evalBdd ::  [Bool] -> BddMgr -> Bdd -> IO Bool
evalBdd varAssigns mgr bdd = do
    let assigns = zip [0..] varAssigns
    res <- foldM (\b (i, v) -> bdd_restrict mgr b i v) bdd assigns
    case res of
        _ | res == bdd_true -> return True
          | res == bdd_false -> return False
          | otherwise -> error ("evalBdd: non-terminal result '" ++ show res ++ "'")

-- | Symbolically evaluates the given Boolean expression with the
-- given BDD manager.
buildBdd :: BddMgr -> BoolExpr -> IO Bdd
buildBdd mgr expr = do
    let bin f l r = do { l' <- go l; r' <- go r; f mgr l' r' }
        go e =
          case e of
              BFalse      -> return bdd_false
              BTrue       -> return bdd_true
              Var i       -> bdd_ith_var mgr (fromIntegral i)
              Not e'      -> bdd_not mgr =<< go e'
              And l r     -> bin bdd_and l r
              Or l r      -> bin bdd_or l r
              Xor l r     -> bin bdd_xor l r
              Equiv l r   -> bin bdd_equiv l r
              Nand l r    -> bin bdd_nand l r
              Implies l r -> bin bdd_implies l r
    go expr
