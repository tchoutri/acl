module ACL.Types.Subject where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Namespace
import ACL.Types.Object
import ACL.Types.Relation (Relation)

data EndSubject = EndSubject
  { namespaceId :: NamespaceId
  , useId :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display EndSubject where
  displayBuilder (EndSubject namespace identifier) = displayBuilder namespace <> ":" <> displayBuilder identifier

data Subject
  = Subject EndSubject
  | SubjectSet SubjectSetTuple
  deriving stock (Eq, Ord, Show)

isEndSubject :: Subject -> Bool
isEndSubject (Subject _) = True
isEndSubject _ = False

instance Display Subject where
  displayBuilder (Subject i) = displayBuilder i
  displayBuilder (SubjectSet subjectSet) = displayBuilder subjectSet

data SubjectSetTuple = SubjectSetTuple
  { object :: Object
  , mRelation :: Maybe Relation
  }
  deriving stock (Eq, Ord, Show)

instance Display SubjectSetTuple where
  displayBuilder (SubjectSetTuple o mr) =
    displayBuilder o <> (maybe "" (\r -> "#" <> displayBuilder r) mr)

objectToSubject :: Object -> Subject
objectToSubject o = Subject (EndSubject o.namespaceId o.identifier)

userToObject :: Subject -> Object
userToObject (Subject (EndSubject namespace userId)) = Object namespace userId
userToObject (SubjectSet (SubjectSetTuple object _)) = object
