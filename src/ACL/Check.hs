module ACL.Check where

import Control.Concurrent.Counter (Counter)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Optics.Core

import ACL.ACLEff
import ACL.Types.CheckError
import ACL.Types.Namespace
import ACL.Types.NamespaceId
import ACL.Types.Object
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.Subject
import ACL.Types.Trace

check :: Map NamespaceId Namespace -> Set RelationTuple -> (Object, Text) -> Subject -> IO (Either CheckError (Bool, (Map RuleName (Seq Text))))
check namespaces relations (obj, rel) user =
  if (RelationTuple obj rel user) `Set.member` relations
    then pure $ Right (True, Map.singleton "direct" (Seq.singleton "_this"))
    else runACL $ check' namespaces relations (obj, rel) user

check'
  :: (Error CheckError :> es, IOE :> es, Reader Counter :> es, State (Map RuleName (Seq Text)) :> es)
  => Map NamespaceId Namespace
  -> Set RelationTuple
  -> (Object, Text)
  -> Subject
  -> Eff es Bool
check' namespaces relations (obj, rel) user =
  case Map.lookup obj.namespaceId namespaces of
    Nothing -> pure False
    Just namespace -> do
      let processRule ruleName rule = do
            haystack <- expandRewriteRules namespaces relations (obj, rel) rule ruleName
            pure $ user `Set.member` haystack
      result <- Map.traverseWithKey processRule namespace.relations
      pure $ or result

expandRewriteRules
  :: (Error CheckError :> es, IOE :> es, Reader Counter :> es, State (Map RuleName (Seq Text)) :> es)
  => Map NamespaceId Namespace
  -> Set RelationTuple
  -> (Object, Text)
  -> RewriteRules
  -> RuleName
  -> Eff es (Set Subject)
expandRewriteRules namespaces relations needle (Union children) ruleName = do
  expanded <- traverse (expandRewriteRuleChild namespaces relations needle ruleName) (Set.toList children)
  pure $
    expanded
      & Set.fromList
      & Set.unions
expandRewriteRules namespaces relations needle (Difference children1 children2) ruleName = do
  set1 <- traverse (expandRewriteRuleChild namespaces relations needle ruleName) (Set.toList children1)
  set2 <- traverse (expandRewriteRuleChild namespaces relations needle ruleName) (Set.toList children2)
  pure $ Set.difference (mconcat set1) (mconcat set2)

expandRewriteRuleChild
  :: (Error CheckError :> es, IOE :> es, Reader Counter :> es, State (Map RuleName (Seq Text)) :> es)
  => Map NamespaceId Namespace
  -> Set RelationTuple
  -> (Object, Text)
  -> RuleName
  -> Child
  -> Eff es (Set Subject)
expandRewriteRuleChild namespaces relationTuples (object, relationName) ruleName = \case
  This targetNamepace -> do
    registerTrace ruleName ("_this " <> display targetNamepace)
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
    registerTrace ruleName ("ComputedSubjectSet on #" <> display relName)
    let filteredRelations = Set.filter (\r -> r.object == object) relationTuples
    pure $ Set.map (\r -> r.subject) filteredRelations
  TupleSetChild computedRelation tuplesetRelation -> do
    registerTrace ruleName (computedRelation <> " from " <> tuplesetRelation)
    -- 1. Fetch all users with the (object, tuplesetRelation) key in relationTuples
    (subjectSet :: Set Subject) <-
      expandRewriteRuleChild namespaces relationTuples (object, tuplesetRelation) ruleName (ComputedSubjectSet computedRelation)
    -- 2. Use these users as new ojects and fetch all users that have a record for <newObjects#computedRelation> in there
    let objectSet = Set.map userToObject subjectSet
    if Set.null objectSet
      then do
        registerTrace ruleName ("Empty objectSet for " <> computedRelation)
        pure $ Set.empty
      else do
        let newObjectsNamespaceId = (Set.elemAt 0 objectSet).namespaceId
            mRewriteRules =
              case Map.lookup newObjectsNamespaceId namespaces of
                Nothing -> Nothing
                Just namespace -> case Map.lookup (RuleName computedRelation) namespace.relations of
                  Nothing -> Nothing
                  Just newRewriteRules -> Just newRewriteRules
         in case mRewriteRules of
              Nothing -> pure Set.empty
              Just rewriteRules -> do
                expanded <- traverse (\o -> expandRewriteRules namespaces relationTuples (o, computedRelation) rewriteRules ruleName) (Set.toList objectSet)
                expanded
                  & Set.fromList
                  & Set.unions
                  & pure
