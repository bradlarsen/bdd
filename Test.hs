module Main (main) where

import BoolExpr (bddSymbolicEvalOk)
import Test.QuickCheck ( quickCheckWith )
import Test.QuickCheck ( stdArgs, maxSize, maxSuccess, maxDiscard )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Printf (hPrintf)

main :: IO ()
main = do
    numTests <- parseArgs
    let qcArgs = stdArgs { maxSize = 20000
                         , maxSuccess = numTests
                         , maxDiscard = numTests
                         }
    quickCheckWith qcArgs bddSymbolicEvalOk

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
