module ACL.ACLEff where

import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Data.Function
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

import ACL.Types.CheckError
import ACL.Types.RewriteRule

type ACLEff =
  Eff
    '[ State (Map RuleName (Seq Text))
     , Error CheckError
     , Reader Counter
     , IOE
     ]

runACL :: ACLEff a -> IO (Either CheckError (a, Map RuleName (Seq Text)))
runACL action = do
  counter <- Counter.new 0
  action
    & State.runState Map.empty
    & Error.runErrorNoCallStack
    & Reader.runReader counter
    & runEff
