module HUnitTests ( hunitTests ) where

import qualified BDD.Raw as Raw

import Control.Exception ( bracket )
import Test.HUnit ( Test(TestList) )

hunitTests :: Test
hunitTests = TestList []
