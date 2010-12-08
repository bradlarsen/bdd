module BoolExpr where

import qualified BDD.Raw as Raw

import Control.Monad (liftM, liftM2, foldM)
import Test.QuickCheck
    (Gen, Arbitrary,
     arbitrary, choose, sized, oneof, frequency, elements)
import Data.Array.IArray (Array, (!), listArray)

-- Boolean expressions
data BoolExpr
    = BFalse
    | BTrue
    | Var Int
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    | Xor BoolExpr BoolExpr
    | Equiv BoolExpr BoolExpr
    | Nand BoolExpr BoolExpr
    | Implies BoolExpr BoolExpr
    deriving (Eq, Show)

arbitraryBoolExpr :: Int -> Int -> Gen BoolExpr
arbitraryBoolExpr numVars = go
    where
        go maxNodes = if maxNodes <= 1 then term else nonterm
            where
                term = if numVars == 0
                       then elements [BFalse, BTrue]
                       else frequency [ (1, return BFalse)
                                      , (1, return BTrue)
                                      , (12, liftM Var (choose (0, numVars - 1)))
                                      ]
                nonterm = oneof (liftM Not (go (maxNodes - 1)) : map bin binCs)
                bin cons = do s1 <- choose (1, maxNodes - 1)
                              let s2 = maxNodes - s1
                              liftM2 cons (go s1) (go s2)
                binCs = [And, Or, Xor, Equiv, Nand, Implies]
                        

-- Boolean expressions along with number of variables
newtype AnnBoolExpr = Ann (Int, BoolExpr)
    deriving (Eq, Show)

arbitraryAnnBoolExpr :: Gen AnnBoolExpr
arbitraryAnnBoolExpr = do
    numVars <- choose (0, 8)
    expr <- sized (arbitraryBoolExpr numVars)
    return $ Ann (numVars, expr)

instance Arbitrary AnnBoolExpr where
    arbitrary = arbitraryAnnBoolExpr

-- Evaluates the given Boolean expression under the given environment.
-- The environment is a list of Boolean values, one for each variable
-- index, in order.
eval :: BoolExpr -> [Bool] -> Bool
eval expression assignment = go expression
    where
        env :: Array Int Bool
        env = listArray (0, length assignment - 1) assignment
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

evalBdd :: Raw.BddMgr -> Raw.Bdd -> [Bool] -> IO Bool
evalBdd mgr bdd varAssigns = do
    let assigns = zip [0..] varAssigns
    res <- foldM (\b (i, v) -> Raw.bdd_restrict mgr b i v) bdd assigns
    case res of
        _ | res == Raw.bdd_true -> return True
          | res == Raw.bdd_false -> return False
          | otherwise -> error "evalBdd: non-terminal result!"

-- | Symbolically evaluates the given Boolean expression with the
-- given BDD manager.
buildBdd :: Raw.BddMgr -> BoolExpr -> IO Raw.Bdd
buildBdd mgr expr = do
    let bin f l r = do { l' <- go l; r' <- go r; f mgr l' r' }
        go e =
          case e of
              BFalse      -> return Raw.bdd_false
              BTrue       -> return Raw.bdd_true
              Var i       -> Raw.bdd_mgr_get_ith_var mgr (fromIntegral i)
              Not e'      -> Raw.bdd_not mgr =<< go e'
              And l r     -> bin Raw.bdd_and l r
              Or l r      -> bin Raw.bdd_or l r
              Xor l r     -> bin Raw.bdd_xor l r
              Equiv l r   -> bin Raw.bdd_equiv l r
              Nand l r    -> bin Raw.bdd_nand l r
              Implies l r -> bin Raw.bdd_implies l r
    go expr
