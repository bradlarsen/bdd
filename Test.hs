module Main (main) where

import QuickCheckProperties
import Test.QuickCheck ( quickCheckWith, Testable )
import Test.QuickCheck ( Args, stdArgs, maxSize, maxSuccess, maxDiscard )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( stderr )
import Text.Printf ( hPrintf )

main :: IO ()
main = do
    numQCTests <- parseArgs
    let qcArgs = stdArgs { maxSize = 1000
                         , maxSuccess = numQCTests
                         , maxDiscard = numQCTests
                         }
    testQuickCheckProperties qcArgs

parseArgs :: IO Int
parseArgs = do
    args <- getArgs
    case args of
        [] -> return 100
        [nt] -> case reads nt of
                    [(i, "")] -> return i
                    _ -> usage
        _ -> usage

usage :: IO a
usage = do
    pName <- getProgName
    _ <- hPrintf stderr "usage: %s NUM_TESTS\n" pName
    exitFailure


testQuickCheckProperties :: Args -> IO ()
testQuickCheckProperties args = do
    putStrLn "Checking QuickCheck properties..."
    let qc :: (Testable a) => a -> IO ()
        qc = quickCheckWith args
    qc prop_bddSymbolicEvalOk
    qc prop_initialNumNodes
    qc prop_correctNumVars
    qc prop_andIdempotent
    qc prop_orIdempotent
    qc prop_selfImplies
    qc prop_selfEquiv
    qc prop_selfXOr
    qc prop_not
    qc prop_doubleNegation
    qc prop_satCount