module ACL.ACLEff where

import Data.Function
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

import ACL.Types.CheckError
import ACL.Types.RewriteRule

type ACLEff =
  Eff
    '[ State (Map RuleName (Seq Text))
     , Error CheckError
     ]

runACL :: ACLEff a -> Either CheckError (a, Map RuleName (Seq Text))
runACL action =
  action
    & State.runState Map.empty
    & Error.runErrorNoCallStack
    & runPureEff
