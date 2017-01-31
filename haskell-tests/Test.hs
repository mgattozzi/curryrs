module Main where

import Curryrs.Types
import Curryrs.Convert
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Rust Function Imports
foreign import ccall "double_input" doubleInput :: I64 -> I64
foreign import ccall "get_true" getTrue :: Boolean
foreign import ccall "get_false" getFalse :: Boolean

main :: IO ()
main = defaultMain tests

-- Test Function Helpers
divTwo :: I64 -> I64
divTwo x = x `div` 2

timesTwo :: I64 -> I64
timesTwo x = x *  2

-- Test Declarations
tests :: TestTree
tests = testGroup "Tests" [unitTests, quickCheckTests]

unitTests = testGroup "Unit Tests"
  [ testCase "Check that double_input works" $
    doubleInput 3 @?= 6,
    testCase "Check that getTrue works" $
    fromBoolean getTrue @?= (Right True),
    testCase "Check that getFalse works" $
    fromBoolean getFalse @?= (Right False)
  ]

quickCheckTests = testGroup "Quickcheck Tests"
  [ testProperty "(divTwo . doubleInput) == id" $
    \x -> (divTwo . doubleInput) x == id x,
    testProperty "doubleInput == timesTwo" $
    \x -> doubleInput x == timesTwo x
  ]
