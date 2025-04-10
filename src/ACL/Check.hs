module ACL.Check where

import Data.Function ((&))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Debug.Trace

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.User

check :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> User -> Bool
check namespaces relations (obj, rel) user =
  (RelationTuple obj rel user) `Set.member` relations
    || check' namespaces relations (obj, rel) user

check' :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> User -> Bool
check' namespaces relations (obj, rel) user =
  case Map.lookup obj.namespaceId namespaces of
    Nothing -> False
    Just namespace ->
      let mRules = Map.lookup rel namespace.relations
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
    let filteredRelations = Set.filter (\r -> r.relationName == relName) relationTuples
     in Set.map (\r -> r.user) (trace (Text.unpack $ "Filtered relations on " <> relName <> ": " <> display (Set.toList filteredRelations)) filteredRelations)
  TupleSetChild computedRelation tuplesetRelation -> undefined

-- 1. Fetch all users for object and tuplesetRelation

-- let (userSet :: Set User) =
--       trace (Text.unpack $ "Computing " <> computedRelation <> " from " <> tuplesetRelation) $
--         expandRewriteRuleChild relationTuples (object, "") (ComputedUserSet tuplesetRelation)
--     (objectSet :: Set Object) =
--       trace (Text.unpack $ "Fetching objects from user refs")
--
--     result =
--       trace (Text.unpack $ "Filtering on " <> computedRelation) $
--         expandRewriteRuleChild relationTuples
--  in result
