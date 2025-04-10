module ACL.Types.RewriteRule where

import Data.Text (Text)
import Data.Vector (Vector)

data RewriteRule
  = Union (Vector Child)
  deriving stock (Eq, Ord, Show)

-- |
-- Examples:
--  * Users (by default, lists all users for the <object,relation> tuple)
--  * ComputedUserSet "editor" to allow editors to view
--  * TupleSet "member" from "subscriber"
data Child
  = This -- _this
  | -- | Relation to the same object
    ComputedUserSet
      Text
  | TupleSetChild
      Text
      -- ^ Indirect relation to match
      Text
      -- ^ Direct relation to match
  deriving stock (Eq, Ord, Show)
