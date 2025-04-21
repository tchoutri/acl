module ACL.Test.RewriteRulesTest.MultipleRestrictionTest where

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
    "Multiple Restrictions"
    [ testCase "Computed subjectset can_write" (testComputedSubjectSetCanWrite fixtures)
    , testCase "Becky can delete document:planning" (testRulesIntersectionCanDeleteBecky fixtures)
    , testCase "Carl cannot delete document:planning" (testRulesIntersectionCanDeleteCarl fixtures)
    ]

fixtures :: (Map NamespaceId Namespace, Set RelationTuple)
fixtures =
  let userNamespace = Namespace "user" Map.empty
      orgNamespace =
        let memberRelation = Single (This "user")
         in Namespace "org" (Map.singleton "member" memberRelation)
      documentNamespace =
        let ownerRelation = Single (This "org")
            writerRelation = Single (This "user")
            canWriteRelation = Single (ComputedSubjectSet "writer")
            canDeleteRelation =
              Intersection
                (Single (ComputedSubjectSet "writer"))
                (Single ("member" `from` "owner"))
         in Namespace
              { namespaceId = "document"
              , relations =
                  Map.fromList
                    [ ("owner", ownerRelation)
                    , ("writer", writerRelation)
                    , ("can_write", canWriteRelation)
                    , ("can_delete", canDeleteRelation)
                    ]
              }
      namespaces =
        Map.fromList
          [ ("user", userNamespace)
          , ("org", orgNamespace)
          , ("document", documentNamespace)
          ]
      abcOrg = Subject $ EndSubject "org" "ABC"
      abcOrgObject = Object "org" "ABC"
      xyzOrgObject = Object "org" "XYZ"
      planningDoc = Object "document" "planning"
      userBecky = Subject $ EndSubject "user" "becky"
      userCarl = Subject $ EndSubject "user" "carl"
      relationTuples =
        Set.fromList
          [ RelationTuple planningDoc "owner" abcOrg
          , RelationTuple planningDoc "writer" userBecky
          , RelationTuple planningDoc "writer" userCarl
          , RelationTuple abcOrgObject "member" userBecky
          , RelationTuple xyzOrgObject "member" userCarl
          ]
   in (namespaces, relationTuples)

testComputedSubjectSetCanWrite :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testComputedSubjectSetCanWrite (namespaces, relationTuples) = do
  let planningDoc = Object "document" "planning"
  let userBecky = Subject $ EndSubject "user" "becky"
  let userCarl = Subject $ EndSubject "user" "carl"
  aclResult0 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_write") userBecky

  assertEqual
    "Becky is not a writer of document:planning!"
    (True, Map.fromList [("can_write", Seq.fromList ["0 | ComputedSubjectSet on #writer"])])
    aclResult0

  aclResult1 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_write") userCarl

  assertEqual
    "Carl is not a writer of document:planning!"
    (True, Map.fromList [("can_write", Seq.fromList ["0 | ComputedSubjectSet on #writer"])])
    aclResult1

testRulesIntersectionCanDeleteBecky :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testRulesIntersectionCanDeleteBecky (namespaces, relationTuples) = do
  let planningDoc = Object "document" "planning"
  let userBecky = Subject $ EndSubject "user" "becky"
  aclResult0 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_delete") userBecky

  assertEqual
    "Becky is not a member of an owner org of document:planning!"
    ( True
    , Map.fromList
        [("can_delete", Seq.fromList ["0 | ComputedSubjectSet on #writer", "1 | member from owner", "2 | ComputedSubjectSet on #member", "3 | _this user", "4 | _this user", "5 | _this user"])]
    )
    aclResult0

testRulesIntersectionCanDeleteCarl :: (Map NamespaceId Namespace, Set RelationTuple) -> Assertion
testRulesIntersectionCanDeleteCarl (namespaces, relationTuples) = do
  let planningDoc = Object "document" "planning"
  let userCarl = Subject $ EndSubject "user" "carl"
  aclResult1 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_delete") userCarl

  assertEqual
    "Carl is a member of an owner org of document:planning!"
    (False, Map.fromList [("can_delete", Seq.fromList ["0 | ComputedSubjectSet on #writer", "1 | member from owner", "2 | ComputedSubjectSet on #member", "3 | _this user", "4 | _this user", "5 | _this user"])])
    aclResult1
