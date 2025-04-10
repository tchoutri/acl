module ACL.Types.User where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Namespace (Namespace)
import ACL.Types.Relation (Relation)

data User = User
  { namespace :: Namespace
  , identifier :: Text
  , mRelation :: Maybe Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display User where
  displayBuilder u =
    displayBuilder u.namespace
      <> ":"
      <> displayBuilder u.identifier
      <> maybe "" (\r -> "#" <> displayBuilder r) u.mRelation
