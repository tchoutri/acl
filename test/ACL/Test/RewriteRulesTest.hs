module ACL.Test.RewriteRulesTest where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit

import ACL.Check
import ACL.Test.Fixtures
import ACL.Test.Utils
import ACL.Types.Namespace
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule

checkTests :: TestTree
checkTests =
  testGroup
    "Check Tests"
    [ testCase "Simple rewrite rule evaluation" testSimpleRewriteRule
    , testCase "Computed Userset" testComputedUserSet
    , testCase "Tuple to Userset" testTupleToUserset
    , testCaseSteps "More complex tuple match" testComplexTupleMatch
    ]

testSimpleRewriteRule :: Assertion
testSimpleRewriteRule = do
  let relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "admin" beatriceAccountUser
          ]

  assertBool
    "Beatrice is not member of SNCF"
    (check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountUser)

testComputedUserSet :: Assertion
testComputedUserSet = do
  let relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "admin" beatriceAccountUser
          ]

  assertEqual
    "Could not find user Beatrice when evaluating computed user set child rule"
    (Set.singleton beatriceAccountUser)
    (expandRewriteRuleChild namespaces relationTuples (sncfOrgObject, "member") (ComputedUserSet "admin"))

  sncfAdminRewriteRules <-
    assertJust $
      Map.lookup ("member" :: Text) organisationNamespace.relations

  assertEqual
    "Could not find user Beatrice for computed user set"
    (Set.singleton beatriceAccountUser)
    (expandRewriteRules namespaces relationTuples (sncfOrgObject, "member") sncfAdminRewriteRules)

  assertBool
    "Beatrice can be seen as a member of SNCF due to being Admin"
    (check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountUser)

testTupleToUserset :: Assertion
testTupleToUserset = do
  let relationTuples =
        Set.fromList
          [ RelationTuple seBankIDFeature "associated_plan" enterprisePlanUser
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgUser
          , RelationTuple sncfOrgObject "admin" charlieAccountUser
          ]

  assertEqual
    "Tupleset Child rule is not correctly expanded"
    (Set.singleton charlieAccountUser)
    (expandRewriteRuleChild namespaces relationTuples (enterprisePlanObject, "subscriber_member") (TupleSetChild "member" "subscriber"))

testComplexTupleMatch :: (String -> IO ()) -> Assertion
testComplexTupleMatch step = do
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

  let step1Relation = RelationTuple seBankIDFeature "associated_plan" enterprisePlanUser
  step (Text.unpack $ "Enterprise plan contains SEBankID (" <> display step1Relation <> ")")
  assertBool
    "Enterprise plan does not grants access to SE Bank ID"
    (check namespaces relationTuples (seBankIDFeature, "associated_plan") enterprisePlanUser)

  let step2Relation = RelationTuple enterprisePlanObject "subscriber" sncfOrgUser
  step (Text.unpack $ "SNCF is subscribed to plan Enterprise (" <> display step2Relation <> ")")
  assertBool
    ("SNCF is not subscribed to Enterprise plan")
    (check namespaces relationTuples (enterprisePlanObject, "subscriber") sncfOrgUser)

  let step3Relation = RelationTuple seBankIDFeature "subscriber" charlieAccountUser
  step (Text.unpack $ "Charlie can access SE Bank ID through SNCF's subscription to Enterprise plan" <> display step3Relation <> ")")
  assertBool
    "Charlie cannot access SE Bank ID through SNCF's subscription to Enterprise plan"
    (check namespaces relationTuples (seBankIDFeature, "can_access") charlieAccountUser)
