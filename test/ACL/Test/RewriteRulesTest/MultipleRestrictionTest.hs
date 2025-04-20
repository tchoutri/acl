module ACL.Test.RewriteRulesTest.MultipleRestrictionTest where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

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
    "Multiple Restrictions"
    [ testCase "Multiple Restrictions model" testMultipleRestrictions
    ]

testMultipleRestrictions :: Assertion
testMultipleRestrictions = do
  let userNamespace = Namespace "user" Map.empty
      orgNamespace =
        let memberRelation = Single (This "user")
         in Namespace "org" (Map.singleton "member" memberRelation)
      documentNamespace =
        let ownerRelation = Single (This "org")
            writerRelation = Single (This "user")
            canWriteRelation = Single (ComputedSubjectSet "writer")
         in Namespace
              { namespaceId = "document"
              , relations =
                  Map.fromList
                    [ ("owner", ownerRelation)
                    , ("writer", writerRelation)
                    , ("can_write", canWriteRelation)
                    ]
              }
  let namespaces =
        Map.fromList
          [ ("user", userNamespace)
          , ("org", orgNamespace)
          , ("document", documentNamespace)
          ]
  let abcOrg = Subject $ EndSubject "org" "ABC"
  let abcOrgObject = Object "org" "ABC"
  let xyzOrgObject = Object "org" "XYZ"
  let planningDoc = Object "document" "planning"
  let userBecky = Subject $ EndSubject "user" "becky"
  let userCarl = Subject $ EndSubject "user" "carl"
  let relationTuples =
        Set.fromList
          [ RelationTuple planningDoc "owner" abcOrg
          , RelationTuple planningDoc "writer" userBecky
          , RelationTuple planningDoc "writer" userCarl
          , RelationTuple abcOrgObject "member" userBecky
          , RelationTuple xyzOrgObject "member" userCarl
          ]

  aclResult0 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_writer") userBecky

  assertEqual
    "Becky is not a writer of document:planning!"
    ( True
    , Map.fromList
        [ ("can_write", Seq.fromList ["0 | ComputedSubjectSet on #writer"])
        , ("owner", Seq.fromList ["1 | _this org"])
        , ("writer", Seq.fromList ["2 | _this user"])
        ]
    )
    aclResult0

  aclResult1 <- assertRight "" =<< check namespaces relationTuples (planningDoc, "can_writer") userBecky

  assertEqual
    "Carl is not a writer of document:planning!"
    ( True
    , Map.fromList
        [ ("can_write", Seq.fromList ["0 | ComputedSubjectSet on #writer"])
        , ("owner", Seq.fromList ["1 | _this org"])
        , ("writer", Seq.fromList ["2 | _this user"])
        ]
    )
    aclResult1
