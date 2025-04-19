module ACL.Test.RewriteRulesTest.FolderDocumentsTest where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
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
    "Folders and Documents"
    [ testCase "Parent folder owner can write document" testParentOwnerFolderCanWriteDocument
    ]

testParentOwnerFolderCanWriteDocument :: Assertion
testParentOwnerFolderCanWriteDocument = do
  let userNamespace = Namespace "user" Map.empty

  let groupRelations = Map.fromList [("member", Union (Set.singleton (This "user")))]
      groupNamespace = Namespace "group" groupRelations

  let folderRelations =
        Map.fromList
          [ ("owner", Union (Set.singleton (This "user")))
          , ("parent", Union (Set.singleton (This "folder")))
          , ("can_create_file", Union (Set.singleton (ComputedSubjectSet "owner")))
          ,
            ( "viewer"
            , Union $
                Set.fromList
                  [ This "user"
                  , "viewer"
                      `from` "parent"
                  ]
            )
          ]
      folderNamespace = Namespace "folder" folderRelations

  let docRelations =
        Map.fromList
          [ ("owner", Union $ Set.singleton (This "user"))
          , ("parent", Union $ Set.singleton (This "folder"))
          ,
            ( "can_write"
            , Union $
                Set.fromList
                  [ ComputedSubjectSet "owner"
                  , "owner" `from` "parent"
                  ]
            )
          ]
      docNamespace = Namespace "doc" docRelations

  let charlesAccountSubject = Subject $ EndSubject "user" "charles"
      bethAccountSubject = Subject $ EndSubject "user" "beth"
      annAccountSubject = Subject $ EndSubject "user" "ann"

  let doc2021RoadmapObject = Object "doc" "2021-roadmap"
      docPublicRoadmapObject = Object "doc" "public-roadmap"

  let folderProduct2021Object = Object "folder" "product-2021"

  let contosoGroupObject = Object "group" "contoso"
      fabrikamObject = Object "group" "fabrikam"

  let folderProduct2021Subject = Subject $ EndSubject "folder" "product-2021"
      fabrikamMembers = SubjectSet $ SubjectSetTuple fabrikamObject (Just "member")
      namespaces =
        Map.fromList
          [ ("group", groupNamespace)
          , ("user", userNamespace)
          , ("folder", folderNamespace)
          , ("doc", docNamespace)
          ]

      relationTuples =
        Set.fromList
          [ RelationTuple doc2021RoadmapObject "parent" folderProduct2021Subject
          , RelationTuple doc2021RoadmapObject "viewer" bethAccountSubject
          , RelationTuple docPublicRoadmapObject "parent" folderProduct2021Subject
          , RelationTuple folderProduct2021Object "owner" annAccountSubject
          , RelationTuple folderProduct2021Object "viewer" fabrikamMembers
          , RelationTuple contosoGroupObject "member" annAccountSubject
          , RelationTuple contosoGroupObject "member" bethAccountSubject
          , RelationTuple fabrikamObject "member" charlesAccountSubject
          ]

  (aclResult, _) <- assertRight "" (runACL (expandRewriteRuleChild namespaces relationTuples (folderProduct2021Object, "owner") "owner" (This "user")))
  assertEqual
    "Unexpected results"
    (Set.singleton (annAccountSubject))
    aclResult

  assertEqual
    "Could not validate doc:2021-roadmap#can_write@user:anne"
    (Right (True, Map.fromList [("can_write", Seq.fromList ["ComputedSubjectSet owner", "owner from parent", "ComputedSubjectSet parent", "_this user"]), ("owner", Seq.fromList ["_this user"]), ("parent", Seq.fromList ["_this folder"])]))
    (check namespaces relationTuples (doc2021RoadmapObject, "can_write") annAccountSubject)
