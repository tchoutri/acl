module ACL.Test.Fixtures where

import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.Relation
import ACL.Types.RewriteRule
import ACL.Types.User

-- type user
--
-- type organization
--   relations
--     define member: [user]
--     define admin: user or member
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

userNamespace :: Namespace
userNamespace =
  Namespace
    { name = "users"
    , relations = Set.empty
    }

beatriceAccountObject :: Object
beatriceAccountObject = Object userNamespace "Beatrice"

charlieAccountObject :: Object
charlieAccountObject = Object userNamespace "Charlie"

lamiaAccountObject :: Object
lamiaAccountObject = Object userNamespace "Lamia"

users :: Vector Object
users =
  Vector.fromList
    [ beatriceAccountObject
    , charlieAccountObject
    , lamiaAccountObject
    ]

beatriceAccountUser :: User
beatriceAccountUser = User "Beatrice" 

charlieAccountUser :: User
charlieAccountUser = User "Charlie" 

lamiaAccountUser :: User
lamiaAccountUser = User "Lamia" 

organisationNamespace :: Namespace
organisationNamespace =
  let r1 =
        Direct $
          MkDirectRelation
            { relationName = "member"
            , rewriteRules =
                Union
                  ( Vector.fromList
                      [ This
                      , ComputedUserSet "admin"
                      ]
                  )
            }
      r2 =
        Direct $
          MkDirectRelation
            { relationName = "admin"
            , rewriteRules = Union $ Vector.singleton (This)
            }
   in Namespace
        { name = "organisation"
        , relations = Set.fromList [r1, r2]
        }

scriveOrgObject :: Object
scriveOrgObject = Object organisationNamespace "scrive"

sncfOrgObject :: Object
sncfOrgObject = Object organisationNamespace "sncf"

trenitaliaOrgObject :: Object
trenitaliaOrgObject = Object organisationNamespace "trenitalia"

organisations :: Vector Object
organisations =
  Vector.fromList
    [ scriveOrgObject
    , sncfOrgObject
    , trenitaliaOrgObject
    ]

scriveOrgUser :: User
scriveOrgUser = 
  let r = Direct $ MkDirectRelation "member" (Union $ Vector.singleton This)
   in UserSet $ UserSetTuple scriveOrgObject (Just r)

sncfOrgUser :: User
sncfOrgUser = User "sncf"

trenitaliaOrgUser :: User
trenitaliaOrgUser = User "trenitalia"

planNamespace :: Namespace
planNamespace =
  let r1 =
        Direct $
          MkDirectRelation
            { relationName = "subscriber"
            , rewriteRules =
                Union (Vector.singleton This)
            }
      r2 =
        TupleSet $
          MkTupleSetRelation
            { relationName = "subscriber_member"
            , tupleSetRelation = "member"
            , userSetRelation = "subscriber"
            }
   in Namespace
        { name = "plan"
        , relations =
            Set.fromList
              [ r1
              , r2
              ]
        }

essentialsPlanObject :: Object
essentialsPlanObject = Object planNamespace "essentials"

businessPlanObject :: Object
businessPlanObject = Object planNamespace "business"

enterprisePlanObject :: Object
enterprisePlanObject = Object planNamespace "enterprise"

planObjects :: Vector Object
planObjects =
  Vector.fromList
    [ essentialsPlanObject
    , businessPlanObject
    , enterprisePlanObject
    ]

essentialsPlanUser :: User
essentialsPlanUser = User "essentials"

businessPlanUser :: User
businessPlanUser = User "business"

enterprisePlanUser :: User
enterprisePlanUser = User "enterprise"

featuresNamespace :: Namespace
featuresNamespace =
  let r1 =
        Direct
          MkDirectRelation
            { relationName = "associated_plan"
            , rewriteRules = Union (Vector.singleton This)
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
            Set.fromList
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
