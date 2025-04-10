module ACL.Types.Relation where

import Data.Text (Text)
import Data.Text.Display
import GHC.Records

import ACL.Types.RewriteRule

data Relation
  = Direct DirectRelation
  | TupleSet TupleSetRelation
  deriving stock (Eq, Ord, Show)

instance Display Relation where
  displayBuilder (Direct r) = displayBuilder r.relationName
  displayBuilder (TupleSet t) = displayBuilder $ t.relationName

instance HasField "relationName" Relation Text where
  getField (Direct directRelation) = directRelation.relationName
  getField (TupleSet tupleSet) = tupleSet.relationName

data DirectRelation = MkDirectRelation
  { relationName :: Text
  , rewriteRules :: RewriteRule
  }
  deriving stock (Eq, Ord, Show)

data TupleSetRelation = MkTupleSetRelation
  { relationName :: Text
  , tupleSetRelation :: Text
  , userSetRelation :: Text
  }
  deriving stock (Eq, Ord, Show)
