module ACL.Types.Subject where

import Data.Text (Text)
import Data.Text.Display
import GHC.Generics
import Optics.Prism

import ACL.Types.NamespaceId (NamespaceId)
import ACL.Types.Object

data EndSubject = EndSubject
  { namespaceId :: NamespaceId
  , useId :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Display EndSubject where
  displayBuilder (EndSubject namespace identifier) = displayBuilder namespace <> ":" <> displayBuilder identifier

data Subject
  = Subject EndSubject
  | SubjectSet SubjectSetTuple
  deriving stock (Eq, Ord, Show)

isEndSubject :: Subject -> Bool
isEndSubject (Subject _) = True
isEndSubject _ = False

_EndSubject :: Prism' Subject EndSubject
_EndSubject =
  prism'
    Subject
    ( \case
        Subject endSubject -> Just endSubject
        SubjectSet _ -> Nothing
    )

instance Display Subject where
  displayBuilder (Subject i) = displayBuilder i
  displayBuilder (SubjectSet subjectSet) = displayBuilder subjectSet

data SubjectSetTuple = SubjectSetTuple
  { object :: Object
  , mRelationName :: Maybe Text
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
