module ACL.Test.Utils where

import Test.Tasty.HUnit qualified as Test
import GHC.Stack

assertJust :: HasCallStack => Maybe a -> IO a
assertJust (Just a) = pure a
assertJust Nothing = Test.assertFailure "Test return Nothing instead of Just"
