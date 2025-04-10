module ACL.Types.User where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.Relation (Relation)

data EndUser = EndUser
  { namespaceId :: NamespaceId
  , useId :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display EndUser where
  displayBuilder (EndUser namespace identifier) = displayBuilder namespace <> ":" <> displayBuilder identifier

data User
  = User EndUser
  | UserSet UserSetTuple
  deriving stock (Eq, Ord, Show)

isEndUser :: User -> Bool
isEndUser (User _) = True
isEndUser _ = False

instance Display User where
  displayBuilder (User i) = displayBuilder i
  displayBuilder (UserSet userSet) = displayBuilder userSet

data UserSetTuple = UserSetTuple
  { object :: Object
  , mRelation :: Maybe Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display UserSetTuple where
  displayBuilder (UserSetTuple o mr) =
    displayBuilder o <> (maybe "" (\r -> "#" <> displayBuilder r) mr)

objectToUser :: Object -> User
objectToUser o = User (EndUser o.namespaceId o.identifier)

userToObject :: User -> Object
userToObject (User (EndUser namespace userId)) = Object namespace userId
userToObject (UserSet (UserSetTuple object _)) = object
