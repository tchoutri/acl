module ACL.Test.DisplayTest where

import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit

import ACL.Test.Fixtures
import ACL.Types.Object (Object (..))
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

spec :: TestTree
spec =
  testGroup
    "Display Tests"
    [ testCase "Display a object-relation-userId tuple" displayObjectRelationSubjectIdRelation
    , testCase "Display a object-relation-subjectSet tuple" displayObjectRelationSubjectSetRelation
    , testCase "Display a union of rewrite rules" displayRewriteRulesUnion
    , testCase "Display a union of single and intersection" displayRewriteRulesUnionOfIntersection
    ]

displayObjectRelationSubjectIdRelation :: Assertion
displayObjectRelationSubjectIdRelation = do
  let scriveOrgObject = Object "org" "scrive"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
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
  let sncfOrgObject = Object "org" "sncf"
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

displayRewriteRulesUnion :: Assertion
displayRewriteRulesUnion = do
  let rule =
        Union
          (Single (This "user"))
          (Single ("viewer" `from` "parent"))

  assertEqual
    "Display instance is not correct"
    "_this user ∪ viewer from parent"
    (display rule)

displayRewriteRulesUnionOfIntersection :: Assertion
displayRewriteRulesUnionOfIntersection = do
  let rule =
        Union
          ( Intersection
              (Single (This "user"))
              (Single ("assignee" `from` "role"))
          )
          (Single (ComputedSubjectSet "editor"))

  assertEqual
    "Display instance is not correct"
    "(_this user ∩ assignee from role) ∪ subjectSet #editor"
    (display rule)
