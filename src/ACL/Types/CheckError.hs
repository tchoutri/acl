module ACL.Types.CheckError where

import Data.Text (Text)

data CheckError
  = EmptyTupleSetRelation Text
  deriving stock (Eq, Ord, Show)
