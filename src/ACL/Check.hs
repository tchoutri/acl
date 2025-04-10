module ACL.Check where

import Data.Function ((&))
import Data.Map.Strict qualified as Map
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
    || check' relations (obj, rel) user

check' :: Set RelationTuple -> (Object, Text) -> User -> Bool
check' relations (obj, rel) user =
  let mRules = Map.lookup rel obj.namespace.relations
   in case mRules of
        Nothing -> False
        Just (rules :: RewriteRules) ->
          let haystack = expandRewriteRule relations (obj, rel) rules
           in user `Set.member` haystack

expandRewriteRule :: Set RelationTuple -> (Object, Text) -> RewriteRules -> Set User
expandRewriteRule relations needle (Union children) =
  children
    & Set.map (expandRewriteRuleChild relations needle)
    & Set.unions

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
