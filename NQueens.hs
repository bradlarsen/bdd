module NQueens (queens) where

import BoolExpr
import Data.List (delete)

conjoin :: [BoolExpr] -> BoolExpr
conjoin = foldr And BTrue

disjoin :: [BoolExpr] -> BoolExpr
disjoin = foldr Or BFalse

-- | Given n, constructs a Boolean expression that encodes the
-- n-queens predicate, along with a decoding function from variable
-- indexes to grid locations.  The grid rows and columns range from 0
-- to n-1.
queens :: Int -> (BoolExpr, Int -> (Int, Int))
queens n =
    let -- convention: grid is row-major
        varAt (r, c) = Var (r * n + c)
        row r = exactlyOne [(r, c) | c <- [0..n-1]]
        col c = exactlyOne [(r, c) | r <- [0..n-1]]
        exactlyOne is = disjoin $ do
            i <- is
            let is' = delete i is
            return (varAt i `And` conjoin (map (Not . varAt) is'))
        rows = map row [0..n-1]
        cols = map col [0..n-1]
        expr = conjoin rows `And` conjoin cols
        decoder i = if i < 0 || i > n^2 - 1
                    then error "queens: invalid variable index"
                    else i `divMod` n
    in (expr, decoder)
