{-# LANGUAGE GADTs #-}
module TestSuite where

import Test.QuickCheck ( Testable, Property, Args, quickCheckWith )
import Text.Printf ( printf )

data Test where
    Test :: (Testable a) => String -> a -> Test
    Suite :: String -> [Test] -> Test

-- FIXME: the output from this isn't formatted well.
runTestWith :: Args -> Test -> IO ()
runTestWith conf = go 0
    where
        go indent test =
            case test of
                Test name prop -> do prIndent indent
                                     printf "%s: " name
                                     quickCheckWith conf prop
                Suite name tests -> do prIndent indent
                                       printf "%s:\n" name
                                       mapM_ (go (indent + 4)) tests
        prIndent indent = putStr (replicate indent ' ')
