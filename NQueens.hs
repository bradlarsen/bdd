module NQueens ( queens, numDistinctSolutions ) where

import BoolExpr
import Control.Monad (guard)
import Data.List (delete)

conjoin :: [BoolExpr] -> BoolExpr
conjoin = foldr And BTrue

disjoin :: [BoolExpr] -> BoolExpr
disjoin = foldr Or BFalse

-- | Returns the number of distinct solutions for the N-queens puzzle
-- with the given N.  These values were obtained from
-- <http://oeis.org/A000170>.
numDistinctSolutions :: Int -> Maybe Int
numDistinctSolutions n = if n <= length nsols
                         then Just (nsols !! (n-1))
                         else Nothing
    where nsols = [ 1
                  , 0
                  , 0
                  , 2
                  , 10
                  , 4
                  , 40
                  , 92
                  , 352
                  , 724
                  , 2680
                  , 14200
                  , 73712
                  , 365596
                  , 2279184
                  , 14772512
                  , 95815104
                  , 666090624
                  , 4968057848
                  , 39029188884
                  , 314666222712
                  , 2691008701644
                  , 24233937684440
                  , 227514171973736
                  , 2207893435808352
                  , 22317699616364044
                  ]

-- | Given n, constructs a Boolean expression that encodes the
-- n-queens predicate, along with a decoding function from variable
-- indexes to grid locations.  The grid rows and columns range from 0
-- to n-1.
queens :: Int -> (BoolExpr, Int -> (Int, Int))
queens n =
    let
        -- convention: grid is row-major
        varAt, nVarAt :: (Int, Int) -> BoolExpr
        varAt (r, c) = Var (r * n + c)
        nVarAt = Not . varAt

        -- builds constraints assuming a queen at the given index
        constrain :: (Int, Int) -> BoolExpr
        constrain idx@(r, c) =
            let v = varAt idx
                rs = [v `Implies` nVarAt (r, c') | c' <- [0..n-1], c' /= c]
                cs = [v `Implies` nVarAt (r', c) | r' <- [0..n-1], r' /= r]
                lr = do i <- [1..n-1]
                        let r' = r + i
                            c' = c + i
                        guard (r' < n && c' < n)
                        return (v `Implies` nVarAt (r', c'))
                ur = do i <- [1..n-1]
                        let r' = r + i
                            c' = c - i
                        guard (r' < n && 0 <= c')
                        return (v `Implies` nVarAt (r', c'))
            in conjoin (rs ++ cs ++ lr ++ ur)

        expr = conjoin [disjoin [varAt (r, c) | c <- [0..n-1]] | r <- [0..n-1]]
                   `And`
               conjoin [constrain (r, c) | r <- [0..n-1], c <- [0..n-1]]

        decoder i = if i < 0 || i > n^2 - 1
                    then error "queens: invalid variable index"
                    else i `divMod` n
    in (expr, decoder)
