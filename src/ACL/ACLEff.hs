module ACL.ACLEff where

import Data.Function
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error

import ACL.Types.CheckError

type ACLEff =
  Eff
    '[ Error CheckError
     ]

runACL :: ACLEff a -> Either CheckError a
runACL action =
  action
    & Error.runErrorNoCallStack
    & runPureEff
