module ACL.Types.Namespace where

import Data.Set (Set)
import Data.Text
import Data.Text.Display

import ACL.Types.Relation (Relation)

data Namespace = Namespace
  { name :: Text
  , relations :: Set Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display Namespace where
  displayBuilder n = displayBuilder n.name
