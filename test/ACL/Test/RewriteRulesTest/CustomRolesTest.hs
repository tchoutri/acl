module ACL.Test.RewriteRulesTest.CustomRolesTest where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

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
    "Custom Roles"
    [ testCase "Ann is assigned to role media-manager" (testThatAnnIsAMediaManager fixtures)
    , testCase "Ann is an editor of asset-category:logos" (testThatAnnIsALogoEditor fixtures)
    , testCase "Beth is not an editor of asset-category:logos" (testThatBethNotIsALogoEditor fixtures)
    ]

fixtures :: (Map NamespaceId Namespace, Set RelationTuple)
fixtures =
  let userNamespace = Namespace "user" Map.empty
      assetCategoryNamespace =
        let viewerRelation =
              Union
                ( Intersection
                    (Single (This "user"))
                    (Single ("assignee" `from` "role"))
                )
                (Single (ComputedSubjectSet "editor"))
            editorRelation =
              Single ("assignee" `from` "user")
         in Namespace
              "asset-category"
              ( Map.fromList
                  [ ("viewer", viewerRelation)
                  , ("editor", editorRelation)
                  ]
              )
      roleNamespace =
        let assigneeRelation = Single (This "user")
         in Namespace
              "role"
              (Map.fromList [("assignee", assigneeRelation)])
      namespaces =
        Map.fromList
          [ ("user", userNamespace)
          , ("asset-category", assetCategoryNamespace)
          , ("role", roleNamespace)
          ]
      mediaManagerRole = Object "role" "media-manager"
      mediaViewerRole = Object "role" "media-viewer"
      userAnn = Subject $ EndSubject "user" "ann"
      userBeth = Subject $ EndSubject "user" "beth"
      logosObject = Object "asset-category" "logos"
      mediaManagers = SubjectSet $ SubjectSetTuple mediaManagerRole (Just "assignee")
      mediaViewers = SubjectSet $ SubjectSetTuple mediaViewerRole (Just "assignee")
      relationTuples =
        Set.fromList
          [ RelationTuple mediaManagerRole "assignee" userAnn
          , RelationTuple mediaViewerRole "assignee" userBeth
          , RelationTuple logosObject "editor" mediaManagers
          , RelationTuple logosObject "viewers" mediaViewers
          ]
   in (namespaces, relationTuples)

testThatAnnIsAMediaManager :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testThatAnnIsAMediaManager (namespaces, relationTuples) = do
  let mediaManagerRole = Object "role" "media-manager"
      userAnn = Subject $ EndSubject "user" "ann"

  aclResult <- assertRight "" =<< check namespaces relationTuples (mediaManagerRole, "assignee") userAnn

  assertEqual
    "Ann is not assigned to role media-manager!"
    (True, Map.fromList [("direct", Seq.fromList ["_this"])])
    aclResult

testThatAnnIsALogoEditor :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testThatAnnIsALogoEditor (namespaces, relationTuples) = do
  let logosObject = Object "asset-category" "logos"
      userAnn = Subject $ EndSubject "user" "ann"

  aclResult <- assertRight "" =<< check namespaces relationTuples (logosObject, "editor") userAnn

  assertEqual
    "Ann is not an editor of asset-category:logos!"
    (True, Map.fromList [("editor", Seq.fromList ["0 | assignee from user", "1 | ComputedSubjectSet on #assignee", "2 | _this user", "3 | _this user"])])
    aclResult

testThatBethNotIsALogoEditor :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testThatBethNotIsALogoEditor (namespaces, relationTuples) = do
  let logosObject = Object "asset-category" "logos"
      userBeth = Subject $ EndSubject "user" "ann"

  aclResult <- assertRight "" =<< check namespaces relationTuples (logosObject, "editor") userBeth

  assertEqual
    "Beth is not an editor of asset-category:logos!"
    (False, Map.fromList [("editor", Seq.fromList ["0 | assignee from user", "1 | ComputedSubjectSet on #assignee", "2 | _this user", "3 | _this user"])])
    aclResult
