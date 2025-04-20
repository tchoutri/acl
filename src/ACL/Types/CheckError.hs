module ACL.Types.CheckError where

import Data.Text (Text)

import ACL.Types.RewriteRule

data CheckError
  = EmptyTupleSetRelation Text
  | NonExistentRule RuleName
  deriving stock (Eq, Ord, Show)
