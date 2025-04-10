module ACL.Types.Relation where

import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)

import ACL.Types.RewriteRule

data Relation
  = Direct DirectRelation
  | TupleSet TupleSetRelation
  deriving stock (Eq, Ord, Show)

instance Display Relation where
  displayBuilder (Direct r) = displayBuilder r.relationName
  displayBuilder (TupleSet t) = displayBuilder $ t.relationName

data DirectRelation = MkDirectRelation
  { relationName :: Text
  , target :: Text
  , rewriteRules :: Vector RewriteRule
  }
  deriving stock (Eq, Ord, Show)

data TupleSetRelation = MkTupleSetRelation
  { relationName :: Text
  , tupleSetRelation :: Text
  , userSetRelation :: Text
  }
  deriving stock (Eq, Ord, Show)
