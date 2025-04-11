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
    , testCase "Computed Subjectset" testComputedSubjectSet
    , testCase "Tuple to Subjectset" testTupleToSubjectset
    , testCaseSteps "More complex tuple match" testComplexTupleMatch
    ]

testSimpleRewriteRule :: Assertion
testSimpleRewriteRule = do
  let relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "admin" beatriceAccountSubject
          , RelationTuple trenitaliaOrgObject "member" charlieAccountSubject
          ]

  assertBool
    "Beatrice is not member of SNCF"
    (check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountSubject)

testComputedSubjectSet :: Assertion
testComputedSubjectSet = do
  let relationTuples =
        Set.fromList
          [ RelationTuple scriveOrgObject "member" lamiaAccountSubject
          , RelationTuple sncfOrgObject "admin" beatriceAccountSubject
          ]

  assertEqual
    "Could not find user Beatrice when evaluating computed user set child rule"
    (Set.singleton beatriceAccountSubject)
    (expandRewriteRuleChild namespaces relationTuples (sncfOrgObject, "member") (ComputedSubjectSet "admin"))

  sncfAdminRewriteRules <-
    assertJust $
      Map.lookup ("member" :: Text) organisationNamespace.relations

  assertEqual
    "Could not find user Beatrice for computed user set"
    (Set.singleton beatriceAccountSubject)
    (expandRewriteRules namespaces relationTuples (sncfOrgObject, "member") sncfAdminRewriteRules)

  assertBool
    "Beatrice can be seen as a member of SNCF due to being Admin"
    (check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountSubject)

testTupleToSubjectset :: Assertion
testTupleToSubjectset = do
  let relationTuples =
        Set.fromList
          [ RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
          , RelationTuple enterprisePlanObject "subscriber" trenitaliaOrgSubject
          , RelationTuple businessPlanObject "subscriber" scriveOrgSubject
          , RelationTuple sncfOrgObject "admin" charlieAccountSubject
          ]

  assertEqual
    "Tupleset Child rule is not correctly expanded"
    (Set.singleton charlieAccountSubject)
    (expandRewriteRuleChild namespaces relationTuples (enterprisePlanObject, "subscriber_member") (TupleSetChild "member" "subscriber"))

testComplexTupleMatch :: (String -> IO ()) -> Assertion
testComplexTupleMatch step = do
  let relationTuples =
        Set.fromList
          [ -- features belonging to plans
            RelationTuple smsFeature "associated_plan" businessPlanSubject
          , RelationTuple smsFeature "associated_plan" enterprisePlanSubject
          , RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
          , RelationTuple noBankIDFeature "associated_plan" enterprisePlanSubject
          , -- accounts belonging to organisations
            RelationTuple scriveOrgObject "member" beatriceAccountSubject
          , RelationTuple sncfOrgObject "admin" beatriceAccountSubject
          , RelationTuple sncfOrgObject "member" charlieAccountSubject
          , RelationTuple trenitaliaOrgObject "member" lamiaAccountSubject
          , -- organisations subscribing to plans
            RelationTuple essentialsPlanObject "subscriber" scriveOrgSubject
          , RelationTuple businessPlanObject "subscriber" trenitaliaOrgSubject
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
          ]

  let step1Relation = RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
  step (Text.unpack $ "Enterprise plan contains SEBankID (" <> display step1Relation <> ")")
  assertBool
    "Enterprise plan does not grants access to SE Bank ID"
    (check namespaces relationTuples (seBankIDFeature, "associated_plan") enterprisePlanSubject)

  let step2Relation = RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
  step (Text.unpack $ "SNCF is subscribed to plan Enterprise (" <> display step2Relation <> ")")
  assertBool
    ("SNCF is not subscribed to Enterprise plan")
    (check namespaces relationTuples (enterprisePlanObject, "subscriber") sncfOrgSubject)

  let step3Relation = RelationTuple seBankIDFeature "subscriber" charlieAccountSubject
  step (Text.unpack $ "Charlie can access SE Bank ID through SNCF's subscription to Enterprise plan" <> display step3Relation <> ")")
  assertBool
    "Charlie cannot access SE Bank ID through SNCF's subscription to Enterprise plan"
    (check namespaces relationTuples (seBankIDFeature, "can_access") charlieAccountSubject)
