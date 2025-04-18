module ACL.Test.RewriteRulesTest.BlocklistTest where

import Control.Monad
import Data.Map.Strict qualified as Map
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
        let editorRelation = Union (Set.fromList [This "user", "member" `from` "team"])
         in Namespace
              { namespaceId = "document"
              , relations = Map.fromList [("editor", editorRelation)]
              }
  let memberRelation = Union (Set.fromList [This "user"])
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
          , RelationTuple teamProduct "member" userBecky
          , RelationTuple teamProduct "member" userCarl
          ]

  productTeamMembers <-
    assertRight "" $
      runACL $
        expandRewriteRules namespaces relationTuples (teamProduct, "member") memberRelation

  assertBool
    "Becky is not part of the members of team:product"
    (Set.member userBecky productTeamMembers)

  assertEqual
    "is user:becky related to document:planning as editor?"
    (Right True)
    (check namespaces relationTuples (documentPlanning, "editor") userBecky)
