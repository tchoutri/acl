module ACL.Examples where

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
