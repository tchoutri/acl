module ACL.Test.Fixtures where

import Data.Map.Strict qualified as Map
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
    , relations = Map.empty
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
beatriceAccountUser = UserId "Beatrice"

charlieAccountUser :: User
charlieAccountUser = UserId "Charlie"

lamiaAccountUser :: User
lamiaAccountUser = UserId "Lamia"

organisationNamespace :: Namespace
organisationNamespace =
  let memberRule = Union (Set.fromList [This, ComputedUserSet "admin"])
      adminRule = Union (Set.singleton (This))
   in Namespace
        { name = "organisation"
        , relations =
            Map.fromList
              [ ("member", memberRule)
              , ("admin", adminRule)
              ]
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
  let r = Relation "member" (Union $ Set.singleton This)
   in UserSet $ UserSetTuple scriveOrgObject (Just r)

sncfOrgUser :: User
sncfOrgUser = UserId "sncf"

trenitaliaOrgUser :: User
trenitaliaOrgUser = UserId "trenitalia"

planNamespace :: Namespace
planNamespace =
  let r1 = Union (Set.singleton This)
      r2 = Union (Set.fromList [TupleSetChild "member" "subscriber"])
   in Namespace
        { name = "plan"
        , relations =
            Map.fromList
              [ ("subscriber", r1)
              , ("subscriber_member", r2)
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
essentialsPlanUser = UserId "essentials"

businessPlanUser :: User
businessPlanUser = UserId "business"

enterprisePlanUser :: User
enterprisePlanUser = UserId "enterprise"

featuresNamespace :: Namespace
featuresNamespace =
  let r1 = Union (Set.singleton This)
      r2 = Union (Set.singleton (TupleSetChild "subscriber_member" "associated_plan"))
   in Namespace
        { name = "features"
        , relations =
            Map.fromList
              [ ("associated_plan", r1)
              , ("can_access", r2)
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
