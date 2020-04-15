module Main (main) where

import qualified System.IO.Silently as Silently
import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import Test.HUnit

import qualified Problem0008
import qualified Problem0009
import qualified Problem0010

tests = TestList [
      "test 8" ~: 5832 ~=? Problem0008.test
    , "test 9" ~: 60   ~=? Problem0009.test
    , "test 10" ~: 17 ~=? Problem0010.test
    ]

main :: IO ()
main = do
    Test.defaultMain $ Test.hUnitTestToTests tests
