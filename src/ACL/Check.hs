module ACL.Check where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.User

check :: Set RelationTuple -> Object -> Text -> User -> Bool
check relations obj rel user =
  (RelationTuple obj rel user) `Set.member` relations
