module ACL.Test.RewriteRulesTest.OrgsPlansTest where

import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Display
import Test.Tasty
import Test.Tasty.HUnit

import ACL.ACLEff
import ACL.Check
import ACL.Test.Fixtures (gatewayFeature, namespaces, noBankIDFeature, organisationNamespace, seBankIDFeature, smsFeature)
import ACL.Test.Utils
import ACL.Types.Namespace
import ACL.Types.NamespaceId
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

spec :: TestTree
spec =
  testGroup
    "Orgs and Plans"
    [ testCase "_this refers to expected subjects" testExpectedSubjectsForThis
    , testCase "Simple rewrite rule evaluation" testSimpleRewriteRule
    , testCase "Computed Subjectset" testComputedSubjectSet
    , testCase "Aggregate subjects that match a relation to an object" testAggregateSubjectsForRelationToObject
    , testCase "Admin of SNCF is properly found as a member and inherits a plan for org:sncf#member" testOrgAdminInheritsMembershipForPlan
    , testCase "Transitive access to a feature by subscribers' members" testTransitiveAccessBySubscriberMembers
    ]

testExpectedSubjectsForThis :: Assertion
testExpectedSubjectsForThis = do
  let sncfOrgObject = Object "org" "sncf"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      kevinTaxInspectorSubject = Subject $ EndSubject "tax_inspector" "Kevin"
      relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "member" beatriceAccountSubject
          , RelationTuple sncfOrgObject "member" charlieAccountSubject
          , RelationTuple sncfOrgObject "member" kevinTaxInspectorSubject
          ]

  aclResult1 <- assertRight "" =<< runACL (expandRewriteRuleChild namespaces relationTuples sncfOrgObject "member" (RuleName "member") (This "user"))

  assertEqual
    "Unexpected result for _this subjects"
    ( Set.fromList
        [ beatriceAccountSubject
        , charlieAccountSubject
        ]
    )
    aclResult1

testSimpleRewriteRule :: Assertion
testSimpleRewriteRule = do
  let sncfOrgObject = Object "org" "sncf"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      trenitaliaOrgObject = Object "org" "trenitalia"
      relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "admin" beatriceAccountSubject
          , RelationTuple trenitaliaOrgObject "member" charlieAccountSubject
          ]

  aclResult1 <- assertRight "" =<< check namespaces relationTuples sncfOrgObject "member" beatriceAccountSubject
  void $
    assertBool
      "Beatrice is not member of SNCF"
      aclResult1

testComputedSubjectSet :: Assertion
testComputedSubjectSet = do
  let sncfOrgObject = Object "org" "sncf"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
      lamiaAccountSubject = Subject $ EndSubject "user" "Lamia"
      scriveOrgObject = Object "org" "scrive"
      relationTuples =
        Set.fromList
          [ RelationTuple scriveOrgObject "member" lamiaAccountSubject
          , RelationTuple sncfOrgObject "admin" beatriceAccountSubject
          ]

  aclResult1 <- assertRight "" =<< runACL (expandRewriteRuleChild namespaces relationTuples sncfOrgObject "member" (RuleName "member") (ComputedSubjectSet "admin"))
  assertEqual
    "Could not find user Beatrice when evaluating computed user set child rule"
    (Set.singleton beatriceAccountSubject)
    aclResult1

  sncfAdminRewriteRules <-
    assertJust $
      Map.lookup (RuleName "member") organisationNamespace.relations

  aclResult2 <- assertRight "" =<< runACL (expandRewriteRules namespaces relationTuples sncfOrgObject "member" (RuleName "member") sncfAdminRewriteRules)
  assertEqual
    "Could not find user Beatrice for computed user set"
    (Set.singleton beatriceAccountSubject)
    aclResult2

  aclResult3 <- assertRight "" =<< check namespaces relationTuples sncfOrgObject "member" beatriceAccountSubject
  void $
    assertBool
      "Beatrice can be seen as a member of SNCF due to being Admin"
      aclResult3

testAggregateSubjectsForRelationToObject :: Assertion
testAggregateSubjectsForRelationToObject = do
  let sncfOrgObject = Object "org" "sncf"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      kevinTaxInspectorSubject = Subject $ EndSubject "tax_inspector" "Kevin"
      relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "member" beatriceAccountSubject
          , RelationTuple sncfOrgObject "member" charlieAccountSubject
          , RelationTuple sncfOrgObject "member" kevinTaxInspectorSubject
          ]
  do
    aclResult <- assertRight "" =<< runACL (interpretRule relationTuples sncfOrgObject "member" (NamespaceId "user"))

    assertEqual
      "Beatrice and Charlie should be the only user members of SNCF"
      (Set.fromList [beatriceAccountSubject, charlieAccountSubject])
      aclResult

testOrgAdminInheritsMembershipForPlan :: Assertion
testOrgAdminInheritsMembershipForPlan = do
  let enterprisePlanObject = Object "plan" "enterprise"
      sncfOrgObject = Object "org" "sncf"
      sncfOrgSubject = Subject $ EndSubject "org" "sncf"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      relationTuples =
        Set.fromList
          [ RelationTuple sncfOrgObject "admin" charlieAccountSubject
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
          ]

  do
    aclResult <-
      assertRight ""
        =<< runACL
          ( expandRewriteRuleChild
              namespaces
              relationTuples
              sncfOrgObject
              "member"
              (RuleName "member")
              (ComputedSubjectSet "admin")
          )

    assertEqual
      "Charlie should be a member of SNCF"
      (Set.singleton charlieAccountSubject)
      aclResult

  do
    aclResult <-
      assertRight ""
        =<< check
          namespaces
          relationTuples
          enterprisePlanObject
          "subscriber"
          sncfOrgSubject

    assertBool
      "SNCF is a subscriber to the Enterprise Plan"
      aclResult

  do
    aclResult <-
      assertRight ""
        =<< check namespaces relationTuples enterprisePlanObject "subscriber_member" charlieAccountSubject

    assertBool
      "Charlie is not a subscriber member to enterprise plan!"
      aclResult

testTransitiveAccessBySubscriberMembers :: Assertion
testTransitiveAccessBySubscriberMembers = do
  let enterprisePlanObject = Object "plan" "enterprise"
      businessPlanObject = Object "plan" "business"
      essentialsPlanObject = Object "plan" "essentials"
      enterprisePlanSubject = Subject $ EndSubject "plan" "enterprise"
      businessPlanSubject = Subject $ EndSubject "plan" "business"
      sncfOrgObject = Object "org" "sncf"
      scriveOrgObject = Object "org" "scrive"
      trenitaliaOrgObject = Object "org" "trenitalia"
      sncfOrgSubject = Subject $ EndSubject "org" "sncf"
      trenitaliaOrgSubject = Subject $ EndSubject "org" "trenitalia"
      scriveOrgSubject = Subject $ EndSubject "org" "scrive"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      lamiaAccountSubject = Subject $ EndSubject "user" "Lamia"
      beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"
      relationTuples =
        Set.fromList
          [ -- features belonging to plans
            RelationTuple smsFeature "associated_plan" businessPlanSubject
          , RelationTuple gatewayFeature "associated_plan" businessPlanSubject
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

  aclResult1 <- assertRight "" =<< check namespaces relationTuples seBankIDFeature "associated_plan" enterprisePlanSubject
  let step1Relation = RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
  assertBool
    (Text.unpack $ "Enterprise plan contains SEBankID (" <> display step1Relation <> ")")
    aclResult1

  aclResult2 <- assertRight "" =<< check namespaces relationTuples enterprisePlanObject "subscriber" sncfOrgSubject
  let step2Relation = RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
  assertBool
    (Text.unpack $ "SNCF is subscribed to plan Enterprise (" <> display step2Relation <> ")")
    aclResult2

  aclResult3 <- assertRight "" =<< check namespaces relationTuples seBankIDFeature "can_access" charlieAccountSubject
  let step3Relation = RelationTuple seBankIDFeature "subscriber" charlieAccountSubject
  void $
    assertBool
      (Text.unpack $ "Charlie can access SE Bank ID through SNCF's subscription to Enterprise plan " <> display step3Relation <> ")")
      aclResult3
