module Main (main) where

import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit

import ACL.Test.Fixtures
import ACL.Test.RewriteRulesTest
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
    [ testCase "Display a object-relation-userId tuple" displayObjectRelationSubjectIdRelation
    , testCase "Display a object-relation-userSet tuple" displayObjectRelationSubjectSetRelation
    ]

displayObjectRelationSubjectIdRelation :: Assertion
displayObjectRelationSubjectIdRelation = do
  let r =
        RelationTuple
          { object = scriveOrgObject
          , relationName = "member"
          , subject = beatriceAccountSubject
          }
  assertEqual
    "Relation is not properly displayed"
    "org:scrive#member@user:Beatrice"
    (display r)

displayObjectRelationSubjectSetRelation :: Assertion
displayObjectRelationSubjectSetRelation = do
  let r =
        RelationTuple
          { object = sncfOrgObject
          , relationName = "member"
          , subject = scriveOrgSubject
          }
  assertEqual
    "Relation is not properly displayed"
    "org:sncf#member@org:scrive#member"
    (display r)
