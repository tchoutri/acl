module ACL.Check where

import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.User

check :: Set RelationTuple -> (Object, Text) -> User -> Bool
check relations (obj, rel) user =
  (RelationTuple obj rel user) `Set.member` relations

check' :: Set RelationTuple -> (Object, Text) -> User -> Bool
check' relations (obj, rel) user =
  let potentialUsers =
        obj.namespace.relations
          & Set.filter (\r -> r.relationName == rel)
          & undefined
   in user `Set.member` potentialUsers

expandRewriteRuleChild :: Set RelationTuple -> (Object, Text) -> Child -> Set User
expandRewriteRuleChild relationTuples (object, relationName) = \case
  This ->
    relationTuples
      & Set.filter (\r -> r.object == object && r.relationName == relationName)
      & Set.map (\r -> r.user)
  ComputedUserSet relName ->
    relationTuples
      & Set.filter (\r -> r.relationName == relName)
      & Set.map (\r -> r.user)
  TupleSetChild _ _ -> undefined
