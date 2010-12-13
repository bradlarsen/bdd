module Main (main) where

import QuickCheckProperties ( bddTests )
import TestSuite ( runTestWith )
import Test.QuickCheck ( Args, stdArgs, maxSize, maxSuccess, maxDiscard )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO ( stderr )
import Text.Printf ( hPrintf )

main :: IO ()
main = do
    numQCTests <- parseArgs
    let qcArgs = stdArgs { maxSize = 250
                         , maxSuccess = numQCTests
                         , maxDiscard = numQCTests
                         }
    runTestWith qcArgs bddTests

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
