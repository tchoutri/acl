module Main (main) where

import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit
import ACL.Test.RewriteRulesTest

import ACL.Test.Fixtures
import ACL.Types.RelationTuple

main :: IO ()
main =
  defaultMain . testGroup "ACL Tests" $ specs

specs :: [TestTree]
specs =
  [ displayTests
  , checkTests
  ]

displayTests :: TestTree
displayTests =
  testGroup
    "Display Tests"
    [ testCase "Display a object-relation-userId tuple" displayObjectRelationUserIdRelation
    , testCase "Display a object-relation-userSet tuple" displayObjectRelationUserSetRelation
    ]

displayObjectRelationUserIdRelation :: Assertion
displayObjectRelationUserIdRelation = do
  let r =
        RelationTuple
          { object = scriveOrgObject
          , relationName = "member"
          , user = beatriceAccountUser
          }
  assertEqual
    "Relation is not properly displayed"
    "organisation:scrive#member@Beatrice"
    (display r)

displayObjectRelationUserSetRelation :: Assertion
displayObjectRelationUserSetRelation = do
  let r =
        RelationTuple
          { object = sncfOrgObject
          , relationName = "member"
          , user = scriveOrgUser
          }
  assertEqual
    "Relation is not properly displayed"
    "organisation:sncf#member@organisation:scrive#member"
    (display r)
