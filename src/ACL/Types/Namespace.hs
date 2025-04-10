module ACL.Types.Namespace where

import Data.Map.Strict (Map)
import Data.String
import Data.Text
import Data.Text.Display

import ACL.Types.RewriteRule

newtype NamespaceId = NamespaceId Text
  deriving newtype (Display, Eq, IsString, Ord, Show)

data Namespace = Namespace
  { namespaceId :: Text
  , relations :: Map Text RewriteRules
  }
  deriving stock (Eq, Ord, Show)

instance Display Namespace where
  displayBuilder n = displayBuilder n.namespaceId
