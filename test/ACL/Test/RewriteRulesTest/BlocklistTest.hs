module ACL.Test.RewriteRulesTest.BlocklistTest where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import ACL.ACLEff
import ACL.Check
import ACL.Test.Utils
import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

spec :: TestTree
spec =
  testGroup
    "Blocklist"
    [ testCase "Implement blocklist model" testThatBlocklistWorks
    ]

testThatBlocklistWorks :: Assertion
testThatBlocklistWorks = do
  let userNamespace = Namespace "user" Map.empty
  let documentNamespace =
        let editorRelation = Difference (RuleSet (Set.fromList [This "user", "member" `from` "team"])) (Single (ComputedSubjectSet "blocked"))
            blockedRelation = Single (This "user")
         in Namespace
              { namespaceId = "document"
              , relations =
                  Map.fromList
                    [ ("editor", editorRelation)
                    , ("blocked", blockedRelation)
                    ]
              }
  let memberRelation = Single (This "user")
  let teamNamespace =
        Namespace
          { namespaceId = "document"
          , relations = Map.fromList [("member", memberRelation)]
          }
  let teamProduct = Object "team" "product"
  let documentPlanning = Object "document" "planning"
  let userBecky = Subject $ EndSubject "user" "becky"
  let userCarl = Subject $ EndSubject "user" "carl"
  let namespaces =
        Map.fromList
          [ ("user", userNamespace)
          , ("document", documentNamespace)
          , ("team", teamNamespace)
          ]
  let relationTuples =
        Set.fromList
          [ RelationTuple documentPlanning "editor" (SubjectSet $ SubjectSetTuple teamProduct (Just "member"))
          , RelationTuple documentPlanning "blocked" userCarl
          , RelationTuple teamProduct "member" userBecky
          , RelationTuple teamProduct "member" userCarl
          ]

  aclResult0 <- assertRight "" =<< runACL (expandRewriteRules namespaces relationTuples (teamProduct, "member") "member" memberRelation)

  assertEqual
    "Becky is not part of the members of team:product!"
    (Set.fromList [Subject (EndSubject "user" "becky"), Subject (EndSubject "user" "carl")])
    aclResult0

  aclResult1 <- assertRight "" =<< check namespaces relationTuples (documentPlanning, "editor") userBecky
  assertBool
    "is user:becky related to document:planning as editor?"
    aclResult1

  aclResult2 <- assertRight "" =<< check namespaces relationTuples (documentPlanning, "editor") userCarl
  assertEqual
    "is user:carl related to document:planning as editor?"
    False
    aclResult2
