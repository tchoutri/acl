module ACL.ACLEff where

import Data.Function
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

import ACL.Types.CheckError

type ACLEff =
  Eff
    '[ State (Seq Text)
     , Error CheckError
     ]

runACL :: ACLEff a -> Either CheckError (a, Seq Text)
runACL action =
  action
    & State.runState Seq.empty
    & Error.runErrorNoCallStack
    & runPureEff
