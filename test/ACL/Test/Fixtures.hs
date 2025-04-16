module ACL.Test.Fixtures
  ( -- ** Organisations

    -- *** Objects
    scriveOrgObject
  , sncfOrgObject
  , trenitaliaOrgObject

    -- *** Subjects
  , scriveOrgSubject
  , sncfOrgSubject
  , trenitaliaOrgSubject

    -- ** Accounts

    -- *** Objects
  , beatriceAccountSubject
  , charlieAccountSubject
  , lamiaAccountSubject
  , namespaces
  , organisationNamespace

    -- ** Features
  , seBankIDFeature
  , smsFeature
  , noBankIDFeature
  , gatewayFeature

    -- ** Plans

    -- *** Objects
  , enterprisePlanObject
  , businessPlanObject
  , essentialsPlanObject

    -- *** Subjects
  , enterprisePlanSubject
  , businessPlanSubject
  ) where

import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.Relation
import ACL.Types.RewriteRule
import ACL.Types.Subject

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

beatriceAccountSubject :: Subject
beatriceAccountSubject = Subject $ EndSubject "user" "Beatrice"

charlieAccountSubject :: Subject
charlieAccountSubject = Subject $ EndSubject "user" "Charlie"

lamiaAccountSubject :: Subject
lamiaAccountSubject = Subject $ EndSubject "user" "Lamia"

organisationNamespace :: Namespace
organisationNamespace =
  let memberRule = Union (Set.fromList [This, ComputedSubjectSet "admin"])
      adminRule = Union (Set.singleton (This))
   in Namespace
        { namespaceId = "org"
        , relations =
            Map.fromList
              [ ("member", memberRule)
              , ("admin", adminRule)
              ]
        }

scriveOrgObject :: Object
scriveOrgObject = Object "org" "scrive"

sncfOrgObject :: Object
sncfOrgObject = Object "org" "sncf"

trenitaliaOrgObject :: Object
trenitaliaOrgObject = Object "org" "trenitalia"

scriveOrgSubject :: Subject
scriveOrgSubject =
  let r = Relation "member" (Union $ Set.singleton This)
   in SubjectSet $ SubjectSetTuple scriveOrgObject (Just r)

sncfOrgSubject :: Subject
sncfOrgSubject = Subject $ EndSubject "org" "sncf"

trenitaliaOrgSubject :: Subject
trenitaliaOrgSubject = Subject $ EndSubject "org" "trenitalia"

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

businessPlanSubject :: Subject
businessPlanSubject = Subject $ EndSubject "plan" "business"

enterprisePlanSubject :: Subject
enterprisePlanSubject = Subject $ EndSubject "plan" "enterprise"

featuresNamespace :: Namespace
featuresNamespace =
  let r1 = Union (Set.singleton This)
      r2 = Union (Set.singleton (TupleSetChild "subscriber_member" "associated_plan"))
   in Namespace
        { namespaceId = "feature"
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

namespaces :: Map NamespaceId Namespace
namespaces =
  Map.fromList
    [ ("user", userNamespace)
    , ("plan", planNamespace)
    , ("feature", featuresNamespace)
    , ("org", organisationNamespace)
    ]
