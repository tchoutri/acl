module ACL.Test.Utils where

import GHC.Stack
import Test.Tasty.HUnit qualified as Test

assertJust :: HasCallStack => Maybe a -> IO a
assertJust (Just a) = pure a
assertJust Nothing = Test.assertFailure "Test return Nothing instead of Just"

assertRight :: (HasCallStack, Show a) => String -> Either a b -> IO b
assertRight _ (Right b) = pure b
assertRight msg (Left a) = Test.assertFailure $ msg <> "\nTest returned `Left " <> show a <> "` instead of `Right`"
