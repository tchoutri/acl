module ACL.Test.Fixtures where

import Data.Map (Map)
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
    { namespaceId = "user"
    , relations = Map.empty
    }

beatriceAccountObject :: Object
beatriceAccountObject = Object "user" "Beatrice"

charlieAccountObject :: Object
charlieAccountObject = Object "user" "Charlie"

lamiaAccountObject :: Object
lamiaAccountObject = Object "user" "Lamia"

users :: Vector Object
users =
  Vector.fromList
    [ beatriceAccountObject
    , charlieAccountObject
    , lamiaAccountObject
    ]

beatriceAccountUser :: User
beatriceAccountUser = User $ EndUser "user" "Beatrice"

charlieAccountUser :: User
charlieAccountUser = User $ EndUser "user" "Charlie"

lamiaAccountUser :: User
lamiaAccountUser = User $ EndUser "user" "Lamia"

organisationNamespace :: Namespace
organisationNamespace =
  let memberRule = Union (Set.fromList [This, ComputedUserSet "admin"])
      adminRule = Union (Set.singleton (This))
   in Namespace
        { namespaceId = "organisation"
        , relations =
            Map.fromList
              [ ("member", memberRule)
              , ("admin", adminRule)
              ]
        }

scriveOrgObject :: Object
scriveOrgObject = Object "organisation" "scrive"

sncfOrgObject :: Object
sncfOrgObject = Object "organisation" "sncf"

trenitaliaOrgObject :: Object
trenitaliaOrgObject = Object "organisation" "trenitalia"

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
sncfOrgUser = User $ EndUser "organisation" "sncf"

trenitaliaOrgUser :: User
trenitaliaOrgUser = User $ EndUser "organisation" "trenitalia"

planNamespace :: Namespace
planNamespace =
  let r1 = Union (Set.singleton This)
      r2 = Union (Set.fromList [TupleSetChild "member" "subscriber"])
   in Namespace
        { namespaceId = "plan"
        , relations =
            Map.fromList
              [ ("subscriber", r1)
              , ("subscriber_member", r2)
              ]
        }

essentialsPlanObject :: Object
essentialsPlanObject = Object "plan" "essentials"

businessPlanObject :: Object
businessPlanObject = Object "plan" "business"

enterprisePlanObject :: Object
enterprisePlanObject = Object "plan" "enterprise"

planObjects :: Vector Object
planObjects =
  Vector.fromList
    [ essentialsPlanObject
    , businessPlanObject
    , enterprisePlanObject
    ]

essentialsPlanUser :: User
essentialsPlanUser = User $ EndUser "plans" "essentials"

businessPlanUser :: User
businessPlanUser = User $ EndUser "plans" "business"

enterprisePlanUser :: User
enterprisePlanUser = User $ EndUser "plans" "enterprise"

featuresNamespace :: Namespace
featuresNamespace =
  let r1 = Union (Set.singleton This)
      r2 = Union (Set.singleton (TupleSetChild "subscriber_member" "associated_plan"))
   in Namespace
        { namespaceId = "features"
        , relations =
            Map.fromList
              [ ("associated_plan", r1)
              , ("can_access", r2)
              ]
        }

smsFeature :: Object
smsFeature = Object "feature" "SMS"

seBankIDFeature :: Object
seBankIDFeature = Object "feature" "SEBankID"

noBankIDFeature :: Object
noBankIDFeature = Object "feature" "NOBankID"

gatewayFeature :: Object
gatewayFeature = Object "feature" "Gateway"

features :: Vector Object
features =
  Vector.fromList
    [ smsFeature
    , seBankIDFeature
    , noBankIDFeature
    ]

namespaces :: Map NamespaceId Namespace
namespaces =
  Map.fromList
    [ ("user", userNamespace)
    , ("plan", planNamespace)
    , ("feature", featuresNamespace)
    , ("organisation", organisationNamespace)
    ]
