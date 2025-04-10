module ACL.Examples where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.Relation
import ACL.Types.RelationTuple
import ACL.Types.RewriteRule
import ACL.Types.User

-- type user
--
-- type organization
--   relations
--     define member: [user]
--
-- type plan
--   relations
--     define subscriber: [organization]
--     define subscriber_member: member from subscriber
--
-- type feature
--   relations
--     define associated_plan: [plan]
--     define can_access: subscriber_member from associated_plan

usersNamespace :: Namespace
usersNamespace =
  Namespace
    { name = "users"
    , relations = Vector.empty
    }

beatriceUser :: Object
beatriceUser = Object usersNamespace "Beatrice"

charlieUser :: Object
charlieUser = Object usersNamespace "Charlie"

lamiaUser :: Object
lamiaUser = Object usersNamespace "Lamia"

users :: Vector Object
users =
  Vector.fromList
    [ beatriceUser
    , charlieUser
    , lamiaUser
    ]

organizationsNamespace :: Namespace
organizationsNamespace =
  let r1 =
        Direct $
          MkDirectRelation
            { relationName = "member"
            , target = "users"
            , rewriteRules = Vector.singleton (Union (Vector.singleton (This)))
            }
   in Namespace
        { name = "organizations"
        , relations = Vector.fromList [r1]
        }

scriveOrgObject :: Object
scriveOrgObject = Object organizationsNamespace "scrive"

sncfOrgObject :: Object
sncfOrgObject = Object organizationsNamespace "sncf"

trenitaliaOrgObject :: Object
trenitaliaOrgObject = Object organizationsNamespace "trenitalia"

organizations :: Vector Object
organizations =
  Vector.fromList
    [ scriveOrgObject
    , sncfOrgObject
    , trenitaliaOrgObject
    ]

scriveOrgUser :: User
scriveOrgUser = User organizationsNamespace "scrive" Nothing

sncfOrgUser :: User
sncfOrgUser = User organizationsNamespace "sncf" Nothing

trenitaliaOrgUser :: User
trenitaliaOrgUser = User organizationsNamespace "trenitalia" Nothing

plansNamespace :: Namespace
plansNamespace =
  let r1 =
        Direct $
          MkDirectRelation
            { relationName = "subscriber"
            , target = "organization"
            , rewriteRules =
                Vector.singleton (Union (Vector.singleton This))
            }
      r2 =
        TupleSet $
          MkTupleSetRelation
            { relationName = "subscriber_member"
            , tupleSetRelation = "member"
            , userSetRelation = "subscriber"
            }
   in Namespace
        { name = "plans"
        , relations =
            Vector.fromList
              [ r1
              , r2
              ]
        }

essentialsPlanObject :: Object
essentialsPlanObject = Object plansNamespace "essentials"

businessPlanObject :: Object
businessPlanObject = Object plansNamespace "business"

enterprisePlanObject :: Object
enterprisePlanObject = Object plansNamespace "enterprise"

planObjects :: Vector Object
planObjects =
  Vector.fromList
    [ essentialsPlanObject
    , businessPlanObject
    , enterprisePlanObject
    ]

essentialsPlanUser :: User
essentialsPlanUser = User plansNamespace "essentials" Nothing

businessPlanUser :: User
businessPlanUser = User plansNamespace "business" Nothing

enterprisePlanUser :: User
enterprisePlanUser = User plansNamespace "enterprise" Nothing

featuresNamespace :: Namespace
featuresNamespace =
  let r1 =
        Direct
          MkDirectRelation
            { relationName = "associated_plan"
            , target = "plan"
            , rewriteRules = Vector.singleton (Union (Vector.singleton This))
            }
      r2 =
        TupleSet
          MkTupleSetRelation
            { relationName = "can_access"
            , tupleSetRelation = "subscriber_member"
            , userSetRelation = "associated_plan"
            }
   in Namespace
        { name = "features"
        , relations =
            Vector.fromList
              [ r1
              , r2
              ]
        }

smsFeature :: Object
smsFeature = Object featuresNamespace "SMS"

seBankIDFeature :: Object
seBankIDFeature = Object featuresNamespace "SEBankID"

noBankIDFeature :: Object
noBankIDFeature = Object featuresNamespace "NOBankID"

gatewayFeature :: Object
gatewayFeature = Object featuresNamespace "Gateway"

features :: Vector Object
features =
  Vector.fromList
    [ smsFeature
    , seBankIDFeature
    , noBankIDFeature
    ]

relationTuples :: Set RelationTuple
relationTuples =
  Set.fromList
    [ -- features belonging to plans
      RelationTuple smsFeature "associated_plan" enterprisePlanUser
    , RelationTuple smsFeature "associated_plan" businessPlanUser
    , RelationTuple seBankIDFeature "associated_plan" businessPlanUser
    , RelationTuple noBankIDFeature "associated_plan" businessPlanUser
    , -- accounts belonging to organisations
      RelationTuple beatriceUser "member" scriveOrgUser
    , RelationTuple charlieUser "member" sncfOrgUser
    , RelationTuple lamiaUser "member" trenitaliaOrgUser
    , -- organisations subscribing to plans
      RelationTuple scriveOrgObject "member" essentialsPlanUser
    , RelationTuple trenitaliaOrgObject "member" businessPlanUser
    , RelationTuple sncfOrgObject "member" enterprisePlanUser
    ]
