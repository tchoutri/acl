module ACL.Types.Relation where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.RewriteRule

data Relation = Relation
  { relationName :: Text
  , rewriteRules :: RewriteRules
  }
  deriving stock (Eq, Ord, Show)

instance Display Relation where
  displayBuilder r = displayBuilder $ r.relationName
