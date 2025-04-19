module ACL.Test.RewriteRulesTest.OrgsPlansTest where

import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
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
    , testCase "Tuple to Subjectset" testTupleToSubjectset
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
  (aclResult1, _) <- assertRight "" =<< (runACL $ expandRewriteRuleChild namespaces relationTuples (sncfOrgObject, "member") (RuleName "member") (This "user"))

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

  aclResult1 <- check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountSubject
  void $
    assertEqual
      "Beatrice is not member of SNCF"
      ( Right
          ( True
          , Map.fromList
              [ ("admin", Seq.fromList ["0 | _this user"])
              , ("member", Seq.fromList ["1 | _this user", "2 | ComputedSubjectSet on #admin"])
              ]
          )
      )
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

  (aclResult1, _) <- assertRight "" =<< (runACL $ expandRewriteRuleChild namespaces relationTuples (sncfOrgObject, "member") (RuleName "member") (ComputedSubjectSet "admin"))
  assertEqual
    "Could not find user Beatrice when evaluating computed user set child rule"
    (Set.singleton beatriceAccountSubject)
    aclResult1

  sncfAdminRewriteRules <-
    assertJust $
      Map.lookup (RuleName "member") organisationNamespace.relations

  (aclResult2, _) <- assertRight "" =<< (runACL $ expandRewriteRules namespaces relationTuples (sncfOrgObject, "member") sncfAdminRewriteRules (RuleName "member"))
  assertEqual
    "Could not find user Beatrice for computed user set"
    (Set.singleton beatriceAccountSubject)
    aclResult2

  aclResult3 <- assertRight "" =<< check namespaces relationTuples (sncfOrgObject, "member") beatriceAccountSubject
  void $
    assertEqual
      "Beatrice can be seen as a member of SNCF due to being Admin"
      (True, Map.fromList [("admin", Seq.fromList ["0 | _this user"]), ("member", Seq.fromList ["1 | _this user", "2 | ComputedSubjectSet on #admin"])])
      aclResult3

testTupleToSubjectset :: Assertion
testTupleToSubjectset = do
  let enterprisePlanObject = Object "plan" "enterprise"
      businessPlanObject = Object "plan" "business"
      enterprisePlanSubject = Subject $ EndSubject "plan" "enterprise"
      sncfOrgObject = Object "org" "sncf"
      sncfOrgSubject = Subject $ EndSubject "org" "sncf"
      trenitaliaOrgSubject = Subject $ EndSubject "org" "trenitalia"
      scriveOrgSubject = Subject $ EndSubject "org" "scrive"
      charlieAccountSubject = Subject $ EndSubject "user" "Charlie"
      relationTuples =
        Set.fromList
          [ RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
          , RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
          , RelationTuple enterprisePlanObject "subscriber" trenitaliaOrgSubject
          , RelationTuple businessPlanObject "subscriber" scriveOrgSubject
          , RelationTuple sncfOrgObject "admin" charlieAccountSubject
          ]

  (aclResult, _) <- assertRight "" =<< (runACL $ expandRewriteRuleChild namespaces relationTuples (enterprisePlanObject, "subscriber_member") "subscriber_member" ("member" `from` "subscriber"))

  assertEqual
    "Tupleset Child rule is not correctly expanded"
    (Set.singleton charlieAccountSubject)
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

  aclResult1 <- assertRight "" =<< check namespaces relationTuples (seBankIDFeature, "associated_plan") enterprisePlanSubject
  let step1Relation = RelationTuple seBankIDFeature "associated_plan" enterprisePlanSubject
  assertEqual
    (Text.unpack $ "Enterprise plan contains SEBankID (" <> display step1Relation <> ")")
    (True, Map.fromList [("direct", Seq.fromList ["_this"])])
    aclResult1

  aclResult2 <- assertRight "" =<< check namespaces relationTuples (enterprisePlanObject, "subscriber") sncfOrgSubject
  let step2Relation = RelationTuple enterprisePlanObject "subscriber" sncfOrgSubject
  assertEqual
    (Text.unpack $ "SNCF is subscribed to plan Enterprise (" <> display step2Relation <> ")")
    (True, Map.fromList [("direct", Seq.fromList ["_this"])])
    aclResult2

  aclResult3 <- assertRight "" =<< check namespaces relationTuples (seBankIDFeature, "can_access") charlieAccountSubject
  let step3Relation = RelationTuple seBankIDFeature "subscriber" charlieAccountSubject
  void $
    assertEqual
      (Text.unpack $ "Charlie can access SE Bank ID through SNCF's subscription to Enterprise plan" <> display step3Relation <> ")")
      ( True
      , Map.fromList
          [ ("associated_plan", Seq.fromList ["0 | _this plan"])
          , ("can_access", Seq.fromList ["1 | subscriber_member from associated_plan", "2 | ComputedSubjectSet on #subscriber_member", "3 | member from subscriber", "4 | ComputedSubjectSet on #member", "5 | _this user", "6 | ComputedSubjectSet on #admin"])
          ]
      )
      aclResult3
