module Test.Types where

import Prelude
import Data.Generic.Rep       (class Generic)
import Data.Generic.Rep.Show  (genericShow)
import Data.Maybe             (Maybe(..))

type Goat =
  { name :: Name
  , age :: Age
  , horns :: Horns
  , contact :: Contact
  , message :: Maybe Message
  }

newtype Name = Name String
derive instance genericName :: Generic Name _
instance showName :: Show Name where
    show = genericShow

newtype Age = Age Int
derive instance genericAge :: Generic Age _
instance showAge :: Show Age where
    show = genericShow

newtype Horns = Horns Int
derive instance genericHorns :: Generic Horns _
instance showHorns :: Show Horns where
    show = genericShow

data Contact
  = ContactEmail Email
  | ContactPhone Phone
derive instance genericContact :: Generic Contact _
instance showContact :: Show Contact where
    show = genericShow

newtype Email = Email String
derive instance genericEmail :: Generic Email _
instance showEmail :: Show Email where
    show = genericShow

newtype Phone = Phone String
derive instance genericPhone :: Generic Phone _
instance showPhone :: Show Phone where
    show = genericShow

newtype Message = Message String
derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
    show = genericShow

type RegisterForm =
  { name :: String
  , age :: String
  , horns :: String
  , email :: String
  , phone :: String
  , contactType :: String
  , message :: String
  }

data ContactType
  = UseEmail
  | UsePhone
