module ACL.Types.RewriteRule where

import Data.Set (Set)
import Data.Text (Text)

data RewriteRules = Union (Set Child)
  deriving stock (Eq, Ord, Show)

-- |
-- Examples:
--  * Users (by default, lists all users for the <object,relation> tuple)
--  * ComputedUserSet "editor" to allow editors to view
--  * TupleSet "member" from "subscriber"
data Child
  = -- | _this
    This
  | -- | Relation to the same object
    ComputedUserSet
      Text
  | TupleSetChild
      Text
      -- ^ Indirect relation to match
      Text
      -- ^ Direct relation to match
  deriving stock (Eq, Ord, Show)
