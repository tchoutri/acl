module ACL.Types.Trace where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)

import ACL.Types.RewriteRule

addToTrace :: RuleName -> Text -> Map RuleName (Seq Text) -> Map RuleName (Seq Text)
addToTrace ruleName segment traces =
  Map.insertWith
    (\newValue oldValue -> oldValue <> newValue)
    ruleName
    (Seq.singleton segment)
    traces
