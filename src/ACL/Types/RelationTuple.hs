module ACL.Types.RelationTuple where

import Data.Text (Text)

import ACL.Types.Object
import ACL.Types.User

data RelationTuple = RelationTuple
  { object :: Object
  , relationName :: Text
  , user :: User
  }
  deriving stock (Eq, Ord, Show)
