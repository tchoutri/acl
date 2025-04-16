module ACL.Check where

import Data.Function ((&))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

check :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Subject -> Bool
check namespaces relations (obj, rel) user =
  (RelationTuple obj rel user) `Set.member` relations
    || check' namespaces relations (obj, rel) user

check' :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Subject -> Bool
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

expandRewriteRules :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> RewriteRules -> Set Subject
expandRewriteRules namespaces relations needle (Union children) =
  children
    & Set.map (expandRewriteRuleChild namespaces relations needle)
    & Set.unions

expandRewriteRuleChild :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Child -> Set Subject
expandRewriteRuleChild namespaces relationTuples (object, relationName) = \case
  This ->
    relationTuples
      & Set.filter (\r -> r.object == object && r.relationName == relationName)
      & Set.map (\r -> r.subject)
  ComputedSubjectSet relName ->
    let filteredRelations = Set.filter (\r -> r.relationName == relName && r.object == object) relationTuples
     in Set.map (\r -> r.subject) filteredRelations
  TupleSetChild computedRelation tuplesetRelation ->
    -- 1. Fetch all users with the (object, tuplesetRelation) key in relationTuples
    let (subjectSet :: Set Subject) =
          expandRewriteRuleChild namespaces relationTuples (object, "") (ComputedSubjectSet tuplesetRelation)
        -- 2. Use these users as new ojects and fetch all users that have a record for <newObjects#computedRelation> in there
        objectSet = Set.map userToObject subjectSet
        newObjectsNamespaceId = (Set.elemAt 0 objectSet).namespaceId
        mRewriteRules =
          case Map.lookup newObjectsNamespaceId namespaces of
            Nothing -> Nothing
            Just namespace -> case Map.lookup computedRelation namespace.relations of
              Nothing -> Nothing
              Just newRewriteRules -> Just newRewriteRules
     in case mRewriteRules of
          Nothing -> Set.empty
          Just rewriteRules ->
            objectSet
              & Set.map (\o -> expandRewriteRules namespaces relationTuples (o, computedRelation) rewriteRules)
              & Set.unions
