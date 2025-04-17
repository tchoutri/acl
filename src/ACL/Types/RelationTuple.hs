module ACL.Types.RelationTuple where

import Data.Text (Text)
import Data.Text.Display
import GHC.Generics

import ACL.Types.Object
import ACL.Types.Subject

data RelationTuple = RelationTuple
  { object :: Object
  , relationName :: Text
  , subject :: Subject
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Display RelationTuple where
  displayBuilder (RelationTuple o r s) =
    displayBuilder o <> "#" <> displayBuilder r <> "@" <> displayBuilder s
