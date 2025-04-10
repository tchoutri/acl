module ACL.Types.Object where

import Data.Text (Text)
import Data.Text.Display

import ACL.Types.Namespace

data Object = Object
  { namespace :: Namespace
  , identifier :: Text
  }
  deriving stock (Eq, Ord, Show)

instance Display Object where
  displayBuilder (Object namespace identifier) = displayBuilder namespace <> ":" <> displayBuilder identifier
