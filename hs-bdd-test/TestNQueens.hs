module Main (main) where

import BoolExpr
import BDD.Raw
import BDD.Util
import NQueens

import Control.Monad (liftM)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import Text.Printf (hPrintf, printf)
import System.IO (stderr)

main :: IO ()
main = getN >>= testQueensEquiv

getN :: IO Int
getN = do
    args <- getArgs
    case args of
        [n] -> case reads n of
                   [(n', "")] -> return n'
                   _ -> usage
        _ -> usage    

usage :: IO a
usage = do
    hPrintf stderr "usage: %s N\n" =<< getProgName
    exitFailure


testQueensEquiv :: Int -> IO ()
testQueensEquiv n = do
    let (expr, _decoder) = queens n
    let nVars = numVars expr :: Int
    withBddMgr nVars $ \mgr -> do
        bdd <- buildBdd mgr expr
        numSols <- liftM realToFrac (bdd_sat_count mgr bdd) :: IO Double
        putStr . show =<< bdd_mgr_get_num_nodes mgr
        putStrLn " nodes in use"
        case numDistinctSolutions n of
            Just s | fromIntegral s /= numSols -> do
                printf "FAILED: wrong number of solutions---expected %d, got %d\n"
                       s (round numSols :: Int)
                exitFailure
            _ -> return ()
