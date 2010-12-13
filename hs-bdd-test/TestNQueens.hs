module Main (main) where

import BoolExpr
import BDD.Raw
import BDD.Util
import NQueens

import Control.Monad (liftM, forM_, when)
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
    let (expr, decoder) = queens n
    let nVars = numVars expr :: Int
        nLines = 2^nVars :: Int
    withBddMgr nVars $ \mgr -> do
        bdd <- buildBdd mgr expr
        numSols <- liftM realToFrac (bdd_sat_count mgr bdd) :: IO Double
        case numDistinctSolutions n of
            Just s | fromIntegral s /= numSols -> do
                printf "FAILED: wrong number of solutions---expected %d, got %d\n"
                       s (round numSols :: Int)
                exitFailure
            _ -> return ()
        -- printf "\n%d: %f solutions\n" n numSols
        -- forM_ (zip [1..] $ assignments nVars) $ \(i, as) -> do
        --      printf "\r%d: %d/%d (%d%%)" n i nLines (i * 100 `div` nLines)
        --      bdd' <- evalBdd as mgr bdd
        --      when (eval as expr /= bdd') $ do
        --          printf "\nFAILED on line %d:\n" i
        --          printAssignment as
        --          exitFailure

printAssignment :: [Bool] -> IO ()
printAssignment as = forM_ (zip [1::Int ..] as) $ \(i, v) -> do
    printf "Var %d: %s\n" i (show v)
