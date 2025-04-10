module ACL.Types.RelationTuple where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Object
import ACL.Types.User

data RelationTuple = RelationTuple
  { object :: Object
  , relationName :: Text
  , user :: User
  }
  deriving stock (Eq, Ord, Show)

instance Display RelationTuple where
  displayBuilder (RelationTuple o r u) =
    displayBuilder o <> "#" <> displayBuilder r <> "@" <> displayBuilder u
