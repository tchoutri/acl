module ACL.Test.RewriteRulesTest.FolderDocumentsTest where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

import ACL.Check
import ACL.Test.Fixtures (userNamespace)
import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

spec :: TestTree
spec =
  testGroup
    "Folders and Documents"
    [ testCase "Parent folder owner can write document" testParentOwnerFolderCanWriteDocument
    ]

testParentOwnerFolderCanWriteDocument :: Assertion
testParentOwnerFolderCanWriteDocument = do
  let groupRelations = Map.fromList [("member", Union (Set.singleton (This "user")))]
      groupNamespace = Namespace "group" groupRelations
      folderProduct2021Subject = Subject $ EndSubject "folder" "product-2021"
      doc2021RoadmapObject = Object "doc" "2021-roadmap"
      bethAccountSubject = Subject $ EndSubject "user" "beth"
      annAccountSubject = Subject $ EndSubject "user" "ann"
      docPublicRoadmapObject = Object "doc" "public-roadmap"
      namespaces =
        Map.fromList
          [("group", groupNamespace)]
      relationTuples =
        Set.fromList
          [ RelationTuple doc2021RoadmapObject "parent" folderProduct2021Subject
          , RelationTuple doc2021RoadmapObject "viewer" bethAccountSubject
          , RelationTuple docPublicRoadmapObject "parent" folderProduct2021Subject
          ]
  assertBool
    (Text.unpack $ "doc:2021-roadmap#can_write@user:anne")
    (check namespaces relationTuples (doc2021RoadmapObject, "can_write") annAccountSubject)
