module ACL.Types.Trace where

import Control.Concurrent.Counter
import Control.Concurrent.Counter qualified as Counter
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Display
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

import ACL.Types.RewriteRule

registerTrace
  :: (IOE :> es, Reader Counter :> es, State (Map RuleName (Seq Text)) :> es)
  => RuleName
  -> Text
  -> Eff es ()
registerTrace ruleName segment = do
  counter <- Reader.ask
  c <- liftIO $ Counter.add counter 1
  State.modify (\s -> addToTrace ruleName (display c <> " | " <> segment) s)

addToTrace :: RuleName -> Text -> Map RuleName (Seq Text) -> Map RuleName (Seq Text)
addToTrace ruleName segment traces =
  Map.insertWith
    (\newValue oldValue -> oldValue <> newValue)
    ruleName
    (Seq.singleton segment)
    traces
