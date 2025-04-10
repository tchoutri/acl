module ACL.Types.User where

import Data.String
import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Object
import ACL.Types.Relation (Relation)

newtype UserId = MkUserId Text
  deriving newtype (Display, Eq, IsString, Ord, Show)

data User
  = UserId UserId
  | UserSet UserSetTuple
  deriving stock (Eq, Ord, Show)

isUserId :: User -> Bool
isUserId (UserId _) = True
isUserId _ = False

instance Display User where
  displayBuilder (UserId i) = displayBuilder i
  displayBuilder (UserSet userSet) = displayBuilder userSet

data UserSetTuple = UserSetTuple
  { object :: Object
  , mRelation :: Maybe Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display UserSetTuple where
  displayBuilder (UserSetTuple o mr) =
    displayBuilder o <> (maybe "" (\r -> "#" <> displayBuilder r) mr)
