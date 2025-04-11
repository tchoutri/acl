module ACL.Types.RewriteRule where

import Data.Set (Set)
import Data.Text (Text)

data RewriteRules = Union (Set Child)
  deriving stock (Eq, Ord, Show)

-- |
-- Examples:
--  * `Viewer: _this` lists all users for the <object,relation> tuple
--  * `Viewer: ComputedSubjectSet "editor"` to allow editors to view
--  * `Viewer:  TupleSet "member" from "subscriber"` to allow members of subscribers to view
data Child
  = -- | _this
    This
  | -- | Relation to the same object
    ComputedSubjectSet
      Text
  | TupleSetChild
      Text
      -- ^ Computed Relation
      Text
      -- ^ Tupleset Relation
  deriving stock (Eq, Ord, Show)
