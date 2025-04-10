module Main (main) where

import Data.Set qualified as Set
import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit

import ACL.Check
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

checkTests :: TestTree
checkTests =
  testGroup
    "Check Tests"
    [ testCase "Simple tuple match" testSimpleTupleMatch
    , testCase "Computed UserSet" testComputedUserSet
    , testCase "More complex tuple match" testComplexTupleMatch
    ]

testSimpleTupleMatch :: Assertion
testSimpleTupleMatch = do
  let relationTuples =
        Set.fromList
          [ RelationTuple smsFeature "associated_plan" enterprisePlanUser
          ]
  assertBool
    "Could not find ACL for simple match"
    (check relationTuples (smsFeature, "associated_plan") enterprisePlanUser)

testComputedUserSet :: Assertion
testComputedUserSet = do
  let relationTuples =
        Set.fromList
          [ RelationTuple beatriceAccountObject "member" scriveOrgUser
          , RelationTuple beatriceAccountObject "admin" sncfOrgUser
          ]
  assertBool
    "Beatrice can be seen as a member of SNCF due to being Admin"
    (check relationTuples (sncfOrgObject, "member") beatriceAccountUser)

testComplexTupleMatch :: Assertion
testComplexTupleMatch = do
  let relationTuples =
        Set.fromList
          [ -- features belonging to plans
            RelationTuple smsFeature "associated_plan" businessPlanUser
          , RelationTuple smsFeature "associated_plan" enterprisePlanUser
          , RelationTuple seBankIDFeature "associated_plan" enterprisePlanUser
          , RelationTuple noBankIDFeature "associated_plan" enterprisePlanUser
          , -- accounts belonging to organisations
            RelationTuple scriveOrgObject "member" beatriceAccountUser
          , RelationTuple sncfOrgObject "admin" beatriceAccountUser
          , RelationTuple sncfOrgObject "member" charlieAccountUser
          , RelationTuple trenitaliaOrgObject "member" lamiaAccountUser
          , -- organisations subscribing to plans
            RelationTuple essentialsPlanObject "subscriber" scriveOrgUser
          , RelationTuple businessPlanObject "subscriber" trenitaliaOrgUser
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgUser
          ]

  assertBool
    "Enterprise plan does not grants access to SE Bank ID"
    (check relationTuples (seBankIDFeature, "associated_plan") enterprisePlanUser)

  assertBool
    "SNCF is not subscribed to Enterprise plan"
    (check relationTuples (sncfOrgObject, "subscriber") enterprisePlanUser)

  assertBool
    "Charlie cannot access SE Bank ID through SNCF's subscription to Enterprise plan"
    (check relationTuples (seBankIDFeature, "can_access") charlieAccountUser)
