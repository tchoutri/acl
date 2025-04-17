module ACL.Check where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Effectful
import Optics.Core

import ACL.Types.Namespace
import ACL.Types.NamespaceId
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

check :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Subject -> Bool
check namespaces relations (obj, rel) user =
  (RelationTuple obj rel user) `Set.member` relations
    || (runPureEff $ check' namespaces relations (obj, rel) user)

check' :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Subject -> Eff es Bool
check' namespaces relations (obj, rel) user =
  case Map.lookup obj.namespaceId namespaces of
    Nothing -> pure False
    Just namespace ->
      let mRules = Map.lookup rel namespace.relations
       in case mRules of
            Nothing -> pure False
            Just (rules :: RewriteRules) -> do
              haystack <- expandRewriteRules namespaces relations (obj, rel) rules
              pure $ user `Set.member` haystack

expandRewriteRules :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> RewriteRules -> Eff es (Set Subject)
expandRewriteRules namespaces relations needle (Union children) = do
  expanded <- traverse (expandRewriteRuleChild namespaces relations needle) (Set.toList children)
  pure $
    expanded
      & Set.fromList
      & Set.unions

expandRewriteRuleChild :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Child -> Eff es (Set Subject)
expandRewriteRuleChild namespaces relationTuples (object, relationName) = \case
  This targetNamepace ->
    relationTuples
      & Set.filter (\r -> r.object == object && r.relationName == relationName && isEndSubject r.subject)
      & Set.foldr'
        ( \r acc ->
            case (r ^. #subject ^? _EndSubject) of
              Just subject -> Set.insert subject acc
              Nothing -> acc
        )
        Set.empty
      & Set.filter (\s -> s.namespaceId == targetNamepace)
      & Set.map Subject
      & pure
  ComputedSubjectSet relName -> do
    let filteredRelations = Set.filter (\r -> r.relationName == relName && r.object == object) relationTuples
    pure $ Set.map (\r -> r.subject) filteredRelations
  TupleSetChild computedRelation tuplesetRelation -> do
    -- 1. Fetch all users with the (object, tuplesetRelation) key in relationTuples
    (subjectSet :: Set Subject) <-
      expandRewriteRuleChild namespaces relationTuples (object, "") (ComputedSubjectSet tuplesetRelation)
    -- 2. Use these users as new ojects and fetch all users that have a record for <newObjects#computedRelation> in there
    let objectSet = Set.map userToObject subjectSet
        newObjectsNamespaceId = (Set.elemAt 0 objectSet).namespaceId
        mRewriteRules =
          case Map.lookup newObjectsNamespaceId namespaces of
            Nothing -> Nothing
            Just namespace -> case Map.lookup computedRelation namespace.relations of
              Nothing -> Nothing
              Just newRewriteRules -> Just newRewriteRules
     in case mRewriteRules of
          Nothing -> pure Set.empty
          Just rewriteRules -> do
            expanded <- traverse (\o -> expandRewriteRules namespaces relationTuples (o, computedRelation) rewriteRules) (Set.toList objectSet)
            expanded
              & Set.fromList
              & Set.unions
              & pure
