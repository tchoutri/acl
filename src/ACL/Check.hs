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
              let haystack = expandRewriteRules namespaces relations (obj, rel) rules
               in user `Set.member` haystack

expandRewriteRules :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> RewriteRules -> Set User
expandRewriteRules namespaces relations needle (Union children) =
  (trace ("Evaluating rules: " <> show children) children)
    & Set.map (expandRewriteRuleChild namespaces relations needle)
    & Set.unions

expandRewriteRuleChild :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Child -> Set User
expandRewriteRuleChild namespaces relationTuples (object, relationName) = \case
  This ->
    relationTuples
      & Set.filter (\r -> r.object == object && r.relationName == relationName)
      & Set.map (\r -> r.user)
  ComputedUserSet relName ->
    let filteredRelations = Set.filter (\r -> r.relationName == relName && r.object == object) relationTuples
     in Set.map (\r -> r.user) (trace (Text.unpack $ "Filtered relations on " <> display object <> "#" <> relName <> "\": " <> display (Set.toList filteredRelations)) filteredRelations)
  TupleSetChild computedRelation tuplesetRelation ->
    -- 1. Fetch all users with the (object, tuplesetRelation) key in relationTuples
    let (userSet :: Set User) =
          trace (Text.unpack $ "Tupleset Relation: " <> display object <> "#" <> tuplesetRelation) $
            expandRewriteRuleChild namespaces relationTuples (object, "") (ComputedUserSet tuplesetRelation)
        -- 2. Use these users as newObjects and fetch all users that have a record for <newObjects#computedRelation> in there
        objectSet = trace ("New objects: " <> show (Set.toList $ Set.map display userSet)) $ Set.map userToObject userSet
        relationRules = undefined
     in objectSet
          & Set.map (\o -> expandRewriteRules namespaces relationTuples (o, computedRelation) relationRules)
          & Set.unions
