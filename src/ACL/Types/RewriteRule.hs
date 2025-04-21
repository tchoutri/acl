module ACL.Types.RewriteRule where

import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)

import ACL.Types.NamespaceId

newtype RuleName = RuleName Text
  deriving newtype (Eq, IsString, Ord, Show)

data RewriteRules
  = -- | A single rule
    Single Child
  | RuleSet (Set Child)
  | -- | Expands all children in the set.
    -- Corresponds to the logical @or (∨)@.
    Union
      RewriteRules
      RewriteRules
  | -- | Return elements of the first set not existing in the second set.
    Difference
      RewriteRules
      RewriteRules
  | -- | Return elements that belong to both sets.
    -- Corresponds to the logical @and (∧)@.
    Intersection
      RewriteRules
      RewriteRules
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
