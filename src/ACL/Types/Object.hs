module ACL.Types.Object where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.NamespaceId (NamespaceId)

data Object = Object
  { namespaceId :: NamespaceId
  , identifier :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display Object where
  displayBuilder (Object namespace identifier) = displayBuilder namespace <> ":" <> displayBuilder identifier
