{-# LANGUAGE OverloadedRecordDot #-}

module ACL.Check where

import Control.Monad.Trace.Class qualified as Tracing
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Trace
import Monitor.Tracing.Zipkin qualified as Tracing
import Optics.Core

import ACL.ACLEff
import ACL.Types.CheckError
import ACL.Types.Namespace
import ACL.Types.NamespaceId
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject

check
  :: Map NamespaceId Namespace
  -> Set RelationTuple
  -> Object
  -> Text
  -> Subject
  -> IO (Either CheckError Bool)
check namespaces relations obj rel user = runACL $ do
  Tracing.rootSpan alwaysSampled ("check " <> display obj <> "#" <> rel <> "@" <> display user) $ do
    if RelationTuple obj rel user `Set.member` relations
      then
        Tracing.childSpan "direct _this" $ pure True
      else check' namespaces relations obj rel user

check'
  :: (Error CheckError :> es, Trace :> es)
  => Map NamespaceId Namespace
  -> Set RelationTuple
  -> Object
  -> Text
  -> Subject
  -> Eff es Bool
check' namespaces relations obj rel user =
  case Map.lookup obj.namespaceId namespaces of
    Nothing -> pure False
    Just namespace -> do
      case Map.lookup (RuleName rel) namespace.relations of
        Nothing -> Error.throwError $ NonExistentRule (RuleName rel)
        Just rules -> do
          Tracing.childSpanWith (Tracing.addInheritedTag "parent_rule" (display rules)) "process_rule" $ do
            haystack <- expandRewriteRules namespaces relations obj rel (RuleName rel) rules
            pure $ user `Set.member` haystack

expandRewriteRules
  :: (Error CheckError :> es, Trace :> es)
  => Map NamespaceId Namespace
  -> Set RelationTuple
  -> Object
  -> Text
  -> RuleName
  -> RewriteRules
  -> Eff es (Set Subject)
expandRewriteRules namespaces relations object relation ruleName rewriteRule = Tracing.childSpan ("Expanding " <> display rewriteRule) $
  case rewriteRule of
    (Single child) ->
      expandRewriteRuleChild namespaces relations object relation ruleName child
    (Union child1 child2) -> do
      set1 <- expandRewriteRules namespaces relations object relation ruleName child1
      set2 <- expandRewriteRules namespaces relations object relation ruleName child2
      let result = Set.unions (Set.fromList [set1, set2])
      pure result
    Difference children1 children2 -> do
      set1 <- expandRewriteRules namespaces relations object relation ruleName children1
      set2 <- expandRewriteRules namespaces relations object relation ruleName children2
      let result = Set.difference set1 set2
      pure result
    Intersection children1 children2 -> do
      set1 <- expandRewriteRules namespaces relations object relation ruleName children1
      set2 <- expandRewriteRules namespaces relations object relation ruleName children2
      let result = Set.intersection set1 set2
      pure result

expandRewriteRuleChild
  :: ( Error CheckError :> es
     , Trace :> es
     )
  => Map NamespaceId Namespace
  -- ^ Namespaces and their relations (env)
  -> Set RelationTuple
  -- ^ Set of rules present in the system (env)
  -> Object
  -- ^ Object of the relation
  -> Text
  -- ^ Relation
  -> RuleName
  -- ^ Rule to expand
  -> Child
  -- ^ Subject set to expand
  -> Eff es (Set Subject)
expandRewriteRuleChild namespaces relationTuples object relationName ruleName = \case
  This targetNamepace -> do
    result <- interpretRule relationTuples object relationName targetNamepace
    Tracing.tag "returned_subjects" (display $ Set.toList result)
    Tracing.tag "amount" (display $ Set.size result)
    Tracing.tag "relation" (display object <> "#" <> display relationName)
    Tracing.tag "rule" (display object <> "#" <> display ruleName)
    pure result
  ComputedSubjectSet relName -> Tracing.childSpan ("computed_subject_set " <> (display object <> "#" <> display relName)) $ do
    let filteredRelations = Set.filter (\r -> r.object == object && r.relationName == relName) relationTuples
    let result =
          filteredRelations
            & Set.map (\r -> r.subject)
    Tracing.tag "returned_subjects" (display $ Set.toList result)
    Tracing.tag "filtered_relations" (display $ Set.toList filteredRelations)
    Tracing.tag "amount" (display $ Set.size result)
    Tracing.tag "relation" (display object <> "#" <> display relationName)
    Tracing.tag "rule" (display object <> "#" <> display ruleName)
    pure result
  TupleSetChild computedRelation tuplesetRelation -> Tracing.childSpan ("TupleSetChild: " <> computedRelation <> " from " <> tuplesetRelation) $ do
    -- 1. Fetch all users with the (object, tuplesetRelation) key in relationTuples
    (subjectSet :: Set Subject) <- Tracing.childSpan "Getting tuple_set" $ do
      Tracing.tag "tuple_set" tuplesetRelation
      let result =
            relationTuples
              & Set.filter (\r -> r.object == object && r.relationName == tuplesetRelation)
              & Set.map (\r -> r.subject)
      Tracing.tag "subject_set" (display $ Set.toList result)
      Tracing.tag "relation_tuples" (display $ Set.toList relationTuples)
      Tracing.tag "filter_by_object" (display object)
      Tracing.tag "filter_by_relation_name" (display computedRelation)
      pure result
    -- 2. Use these users as new ojects and fetch all users that have a record for <newObjects#computedRelation> in there
    let objectSet = Set.map userToObject subjectSet
    if Set.null objectSet
      then do
        Tracing.tag "messsage" ("Empty objectSet for " <> computedRelation)
        pure Set.empty
      else do
        let newObjectsNamespaceId = (Set.elemAt 0 objectSet).namespaceId
            mRewriteRules =
              case Map.lookup newObjectsNamespaceId namespaces of
                Nothing -> Nothing
                Just namespace -> Map.lookup (RuleName computedRelation) namespace.relations
         in case mRewriteRules of
              Nothing -> pure Set.empty
              Just rewriteRules -> do
                expanded <- traverse (\o -> expandRewriteRules namespaces relationTuples o computedRelation ruleName rewriteRules) (Set.toList objectSet)
                let result =
                      expanded
                        & Set.fromList
                        & Set.unions
                Tracing.tag "returned_subjects" (display $ Set.toList result)
                Tracing.tag "amount" (display $ Set.size result)
                Tracing.tag "relation" (display object <> "#" <> display relationName)
                Tracing.tag "rule" (display object <> "#" <> display ruleName)
                pure result

-- | Aggregate all end subjects from 'relationTuples' that match
-- the relation name with the provided object
--
--  Second-level function that is never called by `check`,
--  in order to distinguish between recursive and direct calls
--  to rules.
interpretRule
  :: Set RelationTuple
  -- ^ Known relations
  -> Object
  -- ^ Object of the relation to match on
  -> Text
  -- ^ Rule to match on
  -> NamespaceId
  -- ^ Namespace
  -> Eff es (Set Subject)
interpretRule relationTuples object relationName targetNamepace =
  relationTuples
    & Set.filter (\r -> r.object == object && r.relationName == relationName && isEndSubject r.subject)
    & Set.foldr'
      ( \r acc ->
          case r ^. #subject ^? _EndSubject of
            Just subject -> Set.insert subject acc
            Nothing -> acc
      )
      Set.empty
    & Set.filter (\s -> s.namespaceId == targetNamepace)
    & Set.map Subject
    & pure
