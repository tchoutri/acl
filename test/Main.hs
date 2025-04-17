module Main (main) where

import Test.Tasty

import ACL.Test.DisplayTest qualified as DisplayTest
import ACL.Test.RewriteRulesTest qualified as RewriteRulesTest

main :: IO ()
main =
  defaultMain . testGroup "ACL Tests" $ specs

specs :: [TestTree]
specs =
  [ DisplayTest.spec
  , RewriteRulesTest.spec
  ]
