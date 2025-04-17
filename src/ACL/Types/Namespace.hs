module ACL.Types.Namespace where

import Data.Map.Strict (Map)
import Data.Text
import Data.Text.Display

import ACL.Types.NamespaceId
import ACL.Types.RewriteRule

data Namespace = Namespace
  { namespaceId :: NamespaceId
  , relations :: Map Text RewriteRules
  }
  deriving stock (Eq, Ord, Show)

instance Display Namespace where
  displayBuilder n = displayBuilder n.namespaceId
