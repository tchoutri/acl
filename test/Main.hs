module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Exception
import System.Exit
import System.IO
import Test.Tasty

import ACL.Test.DisplayTest qualified as DisplayTest
import ACL.Test.RewriteRulesTest qualified as RewriteRulesTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (defaultMain . testGroup "ACL Tests" $ specs)
    `catch` ( \(e :: ExitCode) -> do
                Concurrent.threadDelay 3_000_000
                throwIO e
            )

specs :: [TestTree]
specs =
  [ DisplayTest.spec
  , RewriteRulesTest.spec
  ]
