module ACL.Test.RewriteRulesTest.BlocklistTest where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import ACL.ACLEff
import ACL.Check
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
    "Blocklist"
    [ testCase "Checking that team:product#member@user:becky" testThatBeckyIsMemberOfTeamProduct
    , testCase "Checking that document:planning#editor@user:becky" testThatBeckyIsEditorOfDocumentPlanning
    , testCase "Checking that document:planning#editor@user:carl is false" testThatCarlIsBlockedFromEditingDocumentPlanning
    ]

testThatBeckyIsMemberOfTeamProduct :: Assertion
testThatBeckyIsMemberOfTeamProduct = do
  aclResult <-
    assertRight ""
      =<< runACL
        ( expandRewriteRules
            namespaces
            relationTuples
            teamProduct
            "member"
            (RuleName "member")
            memberRelation
        )

  assertEqual
    "Becky should be part of the members of team:product"
    (Set.fromList [Subject (EndSubject "user" "becky"), Subject (EndSubject "user" "carl")])
    aclResult

testThatBeckyIsEditorOfDocumentPlanning :: Assertion
testThatBeckyIsEditorOfDocumentPlanning = do
  aclResult <-
    assertRight ""
      =<< runACL
        ( expandRewriteRules
            namespaces
            relationTuples
            documentPlanning
            "editor"
            (RuleName "editor")
            editorRelation
        )

  assertEqual
    ""
    (Set.fromList [userBecky])
    aclResult

testThatCarlIsBlockedFromEditingDocumentPlanning :: Assertion
testThatCarlIsBlockedFromEditingDocumentPlanning = do
  aclResult <- assertRight "" =<< check namespaces relationTuples documentPlanning "editor" userCarl

  assertEqual
    "is user:carl related to document:planning as editor?"
    False
    aclResult

userNamespace :: Namespace
userNamespace = Namespace "user" Map.empty

blockedRelation :: RewriteRules
blockedRelation = Single (This "user")

memberRelation :: RewriteRules
memberRelation = Single (This "user")

editorRelation :: RewriteRules
editorRelation =
  Difference
    ( Union
        (Single (This "user"))
        (Single ("member" `from` "team"))
    )
    (Single (ComputedSubjectSet "blocked"))

documentNamespace :: Namespace
documentNamespace =
  Namespace
    { namespaceId = "document"
    , relations =
        Map.fromList
          [ ("editor", editorRelation)
          , ("blocked", blockedRelation)
          ]
    }

teamNamespace :: Namespace
teamNamespace =
  Namespace
    { namespaceId = "team"
    , relations = Map.fromList [("member", memberRelation)]
    }

teamProduct :: Object
teamProduct = Object "team" "product"

documentPlanning :: Object
documentPlanning = Object "document" "planning"

userBecky :: Subject
userBecky = Subject $ EndSubject "user" "becky"

userCarl :: Subject
userCarl = Subject $ EndSubject "user" "carl"

namespaces :: Map.Map NamespaceId Namespace
namespaces =
  Map.fromList
    [ ("user", userNamespace)
    , ("document", documentNamespace)
    , ("team", teamNamespace)
    ]

relationTuples :: Set.Set RelationTuple
relationTuples =
  Set.fromList
    [ RelationTuple documentPlanning "editor" (SubjectSet $ SubjectSetTuple teamProduct (Just "member")) -- document:planning#editor@team:product#member
    , RelationTuple documentPlanning "blocked" userCarl
    , RelationTuple teamProduct "member" userBecky
    , RelationTuple teamProduct "member" userCarl
    ]
