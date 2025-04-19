module ACL.Test.RewriteRulesTest.BlocklistTest where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Display
import Debug.Trace
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
        let editorRelation = Union (Set.fromList [This "user", "member" `from` "team"])
            blockedRelation = Union (Set.singleton (This "user"))
         in Namespace
              { namespaceId = "document"
              , relations =
                  Map.fromList
                    [ ("editor", editorRelation)
                    , ("blocked", blockedRelation)
                    ]
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

  aclResult0 <- assertRight "" =<< (runACL $ expandRewriteRules namespaces relationTuples (teamProduct, "member") memberRelation "member")

  assertEqual
    "Becky is not part of the members of team:product!"
    (Set.fromList [Subject (EndSubject "user" "becky"), Subject (EndSubject "user" "carl")], Map.fromList [("member", Seq.fromList ["0 | _this user"])])
    aclResult0

  traceM $ Text.unpack $ "relation tuples: " <> (display $ Set.toList relationTuples)
  aclResult1 <- assertRight "" =<< check namespaces relationTuples (documentPlanning, "editor") userBecky
  assertEqual
    "is user:becky related to document:planning as editor?"
    ( True
    , Map.fromList
        [ ("blocked", Seq.fromList ["0 | _this user"])
        , ("editor", Seq.fromList ["1 | _this user", "2 | member from team", "3 | ComputedSubjectSet on #member", "4 | _this user"])
        ]
    )
    aclResult1
