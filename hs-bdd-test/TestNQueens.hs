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
import Data.IORef (newIORef, readIORef, writeIORef)

main :: IO ()
main = getParams >>= testQueensEquiv

getParams :: IO Int
getParams = do
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

-- testQueensEquiv :: Int -> IO ()
-- testQueensEquiv n = do
--     let (expr, _decoder) = queens n
--     let nVars = numVars expr :: Int
--     withBddMgr nVars $ \mgr -> do
--         bdd <- buildBdd mgr expr
--         numSols <- liftM realToFrac (bdd_sat_count mgr bdd) :: IO Double
--         putStr . show =<< bdd_mgr_get_num_nodes mgr
--         putStrLn " nodes in use"
--         case numDistinctSolutions n of
--             Just s | fromIntegral s /= numSols -> do
--                 printf "FAILED: wrong number of solutions---expected %d, got %d\n"
--                        s (round numSols :: Int)
--                 exitFailure
--             _ -> return ()

testQueensEquiv :: Int -> IO ()
testQueensEquiv n = do
    let nVars = n * n
    withBddMgrHint nVars 1000000 $ \mgr -> do
        bdd <- buildQueens n mgr
        numSols <- liftM realToFrac (bdd_sat_count mgr bdd) :: IO Double
        putStr . show =<< bdd_mgr_get_num_nodes mgr
        putStrLn " nodes in use"
        case numDistinctSolutions n of
            Just s | fromIntegral s /= numSols -> do
                printf "FAILED: wrong number of solutions---expected %d, got %d\n"
                       s (round numSols :: Int)
                exitFailure
            _ -> return ()


-- This is nearly a verbatim translation from the C++ N-queens
-- encoding in the BuDDy 2.4 examples.  Way more efficient than my
-- earlier encoding, for some reason.
--
-- The number of nodes does not match the BuDDy queens example
-- exactly, but if garbage collection is (effectively) disabled in
-- BuDDy, the counts are within 100 for N up to 12.
--
-- For the N-Queens problem, garbage collection makes a big
-- difference!
buildQueens :: Int -> BddMgr -> IO Bdd
buildQueens nVars mgr = do
    let n = fromIntegral nVars
        loc i j = bdd_mgr_get_ith_var mgr (i * n + j)

    queen <- newIORef bdd_true

    -- place a queen in each row
    forM_ [0..n-1] $ \i -> do
        e <- newIORef bdd_false
        forM_ [0..n-1] $ \j -> do
            eVal <- readIORef e
            writeIORef e =<< bdd_or mgr eVal =<< loc i j
        queenVal <- readIORef queen
        writeIORef queen =<< bdd_and mgr queenVal =<< readIORef e

    -- build requirements for each variable
    forM_ [0..n-1] $ \i -> do
        forM_ [0..n-1] $ \j -> do
            hPrintf stderr "Adding position %s,%s\n" (show i) (show j)

            a <- newIORef bdd_true
            b <- newIORef bdd_true
            c <- newIORef bdd_true
            d <- newIORef bdd_true

            forM_ [0..n-1] $ \k -> do
                when (k /= j) $ do
                    aVal <- readIORef a
                    xij <- loc i j
                    nxik <- bdd_not mgr =<< loc i k
                    writeIORef a =<< bdd_and mgr aVal =<< bdd_implies mgr xij nxik

            forM_ [0..n-1] $ \k -> do
                when (k /= i) $ do
                    bVal <- readIORef b
                    xij <- loc i j
                    nxkj <- bdd_not mgr =<< loc k j
                    writeIORef b =<< bdd_and mgr bVal =<< bdd_implies mgr xij nxkj

            forM_ [0..n-1] $ \k -> do
                let ll = k - i + j;
                when (ll >= 0 && ll < n && k /= i) $ do
                    cVal <- readIORef c
                    xij <- loc i j
                    nxkll <- bdd_not mgr =<< loc k ll
                    writeIORef c =<< bdd_and mgr cVal =<< bdd_implies mgr xij nxkll

            forM_ [0..n-1] $ \k -> do
                let ll = i + j - k
                when (ll >= 0 && ll < n && k /= i) $ do
                    dVal <- readIORef d
                    xij <- loc i j
                    nxkll <- bdd_not mgr =<< loc k ll
                    writeIORef d =<< bdd_and mgr dVal =<< bdd_implies mgr xij nxkll

            abcd <- newIORef =<< readIORef a
            abcdVal <- readIORef abcd
            writeIORef abcd =<< bdd_and mgr abcdVal =<< readIORef b
            abcdVal <- readIORef abcd
            writeIORef abcd =<< bdd_and mgr abcdVal =<< readIORef c
            abcdVal <- readIORef abcd
            writeIORef abcd =<< bdd_and mgr abcdVal =<< readIORef d
            queenVal <- readIORef queen
            writeIORef queen =<< bdd_and mgr queenVal =<< readIORef abcd

    readIORef queen
