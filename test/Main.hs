module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import ACL.Check
import ACL.Examples

main :: IO ()
main =
  defaultMain . testGroup "ACL Tests" $ specs

specs :: [TestTree]
specs =
  [ testCase "Simple tuple match" testSimpleTupleMatch
  ]

testSimpleTupleMatch :: Assertion
testSimpleTupleMatch = do
  assertBool
    "Could not find ACL for simple match"
    (check relationTuples smsFeature "associated_plan" enterprisePlanUser)
