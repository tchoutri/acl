module ACL.Types.RewriteRule where

import Data.Set (Set)
import Data.Text (Text)

import ACL.Types.NamespaceId

data RewriteRules = Union (Set Child)
  deriving stock (Eq, Ord, Show)

-- |
-- Examples:
--  * `Viewer: _this` lists all users for the <object,relation> tuple
--  * `Viewer: ComputedSubjectSet "editor"` to allow editors to view
--  * `Viewer:  TupleSet "member" from "subscriber"` to allow members of subscribers to view
data Child
  = -- | `_this` represents all subjects with the '<object>#<relation>' pair.
    This
      NamespaceId
      -- ^ Namespace for the subjects
  | -- | Relation to the same object
    ComputedSubjectSet
      Text
  | TupleSetChild
      Text
      -- ^ Computed Relation
      Text
      -- ^ Tupleset Relation
  deriving stock (Eq, Ord, Show)

-- | Use it like this: "member" `from` "team"
from :: Text -> Text -> Child
from computedRelation tuplesetRelation = TupleSetChild computedRelation tuplesetRelation
