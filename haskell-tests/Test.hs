module Main where

import Types
import Convert
import Test.Tasty
import Test.Tasty.HUnit

foreign import ccall "double_input" doubleInput :: I32 -> I32
foreign import ccall "get_true" getTrue :: Boolean
foreign import ccall "get_false" getFalse :: Boolean

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit Tests"
  [ testCase "Check that double_input works" $
    doubleInput 3 @?= 6,
    testCase "Check that getTrue works" $
    fromBoolean getTrue @?= (Right True),
    testCase "Check that getFalse works" $
    fromBoolean getFalse @?= (Right False)
  ]
