module ACL.ACLEff where

import Data.Function
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Trace
import Effectful.Trace qualified as Tracing
import Monitor.Tracing.Zipkin

import ACL.Tracing qualified as Tracing
import ACL.Types.CheckError

type ACLEff =
  Eff
    '[ Trace
     , Error CheckError
     , IOE
     ]

runACL :: ACLEff a -> IO (Either CheckError a)
runACL action = do
  zipkin <- liftIO $ Tracing.newZipkin "localhost"
  action
    & Tracing.runTrace zipkin.zipkinTracer
    & Error.runErrorNoCallStack
    & runEff
