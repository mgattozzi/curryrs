module Main where

import Curryrs.Types
import Curryrs.Convert
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Rust Function Imports
foreign import ccall "double_input" doubleInput :: U64 -> U64
foreign import ccall "get_true" getTrue :: Boolean
foreign import ccall "get_false" getFalse :: Boolean

main :: IO ()
main = defaultMain tests

-- Test Function Helpers
divTwo :: Maybe U64 -> Maybe U64
divTwo x = case x of
  Just y -> Just $ y `div` 2
  Nothing -> Nothing

timesTwo :: U64 -> Maybe U64
timesTwo x
  -- This number or higher causes the value to wrap to a negative value
  | x >= 4611686018427387904 = Nothing
  | otherwise = Just $ x * 2

-- Wrapped Rust code variant
inputDouble :: U64 -> Maybe U64
inputDouble x
  -- This number or higher causes the value to wrap to a negative value
  | x >= 4611686018427387904 = Nothing
  | otherwise = Just $ doubleInput x

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
  [
    testProperty "(divTwo . inputDouble) == id" $
    \x -> case (divTwo . inputDouble) x of
      Just y -> y == id x
      Nothing -> True,

    testProperty "doubleInput == timesTwo" $
    \x ->  case [inputDouble x, timesTwo x] of
      [Just x, Just y] -> x == y
      [Nothing, Nothing] -> True
      otherwise -> False
  ]
