module ACL.Test.RewriteRulesTest where

import Test.Tasty

import ACL.Test.RewriteRulesTest.BlocklistTest qualified as BlocklistTest
import ACL.Test.RewriteRulesTest.FolderDocumentsTest qualified as FolderDocumentsTest
import ACL.Test.RewriteRulesTest.OrgsPlansTest qualified as OrgsPlansTest

spec :: TestTree
spec =
  testGroup
    "Rewrite Rules"
    [ OrgsPlansTest.spec
    , FolderDocumentsTest.spec
    , BlocklistTest.spec
    ]
