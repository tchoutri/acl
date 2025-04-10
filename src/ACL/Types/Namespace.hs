module ACL.Types.Namespace where

import Data.Text
import Data.Text.Display
import Data.Vector (Vector)

import ACL.Types.Relation (Relation)

data Namespace = Namespace
  { name :: Text
  , relations :: Vector Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display Namespace where
  displayBuilder n = displayBuilder n.name
