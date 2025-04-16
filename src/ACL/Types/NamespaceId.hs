module ACL.Types.NamespaceId where

import Data.String
import Data.Text
import Data.Text.Display

newtype NamespaceId = NamespaceId Text
  deriving newtype (Display, Eq, IsString, Ord, Show)
