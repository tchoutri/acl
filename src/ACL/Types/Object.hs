module ACL.Types.Object where

import Data.Text (Text)

import ACL.Types.Namespace

data Object = Object
  { namespace :: Namespace
  , identifier :: Text
  }
  deriving stock (Eq, Ord, Show)
