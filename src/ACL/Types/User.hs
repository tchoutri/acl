module ACL.Types.User where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Object
import ACL.Types.Relation (Relation)

data User
  = User Text
  | UserSet UserSetTuple
  deriving stock (Eq, Ord, Show)

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
