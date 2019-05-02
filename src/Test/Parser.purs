module Goat.Parser where

import Prelude                                (bind, not, pure, ($), (<$>), (<*>), (<<<), (<=), (>=))
import Control.Monad.Transformerless.Except   (runExcept)
import Data.Either                            (Either)
import Data.Maybe                             (Maybe(..))
import Data.Int                               (fromString)
import Data.List                              (List)
import Data.String                          as Str
import Text.Parsing.Simple                    (Parser, (|=), (<?>))
import Text.Parsing.Simple                  as Parser
import Test.Types                             (Age(..), Contact(..), ContactType(..), Email(..), Goat, Horns(..), Message(..), Name(..), Phone(..), RegisterForm)

toGoat :: RegisterForm -> Either (List Parser.ParseError) Goat
toGoat = runExcept <<< (Parser.parse goat)

goat :: Parser RegisterForm Goat
goat = { name: _, age: _, horns: _, contact: _, message: _ }
  <$> name_
  <*> age_
  <*> horns_
  <*> contact_
  <*> message_

name_ :: forall r. Parser { name :: String | r } Name
name_ = Name <$> ((_.name <$> Parser.stream) |= (not <<< Str.null) <?> "NameRequired")

age_ :: forall r. Parser { age :: String | r } Age
age_ = Age <$> do
  digit <- (_.age <$> Parser.stream) |= (not <<< Str.null) <?> "AgeRequired"
  age <- case fromString digit of
    Just i  -> pure i
    Nothing -> Parser.fail "AgeInvalidInt"
  if age >= 0
    then pure age
    else Parser.fail "AgeNegative"

horns_ :: forall r. Parser { horns :: String | r } Horns
horns_ = Horns <$> do
  digit <- (_.horns <$> Parser.stream) |= (not <<< Str.null) <?> "HornsRequired"
  horns <- case fromString digit of
    Just i  -> pure i
    Nothing -> Parser.fail "HornsInvalidInt"
  if horns >= 0
    then if horns <= 2
      then pure horns
      else Parser.fail "HornsTooMany"
    else Parser.fail "HornsNegative"

contact_ :: forall r. Parser { contactType :: String, email :: String, phone :: String | r } Contact
contact_ = do
  cTypeStr <- (_.contactType <$> Parser.stream) |= (not <<< Str.null) <?> "ContactTypeRequired"
  ctype <- case cTypeStr of
    "ContactEmail" -> pure UseEmail
    "ContactPhone" -> pure UsePhone
    _              -> Parser.fail "ContactTypeInvalid"
  case ctype of
    UseEmail -> ContactEmail <$> email
    UsePhone -> ContactPhone <$> phone
  where
    email :: forall s. Parser { email :: String | s } Email
    email = Email <$> ((_.email <$> Parser.stream) |= (not <<< Str.null) <?> "EmailRequired")
    phone :: forall t. Parser { phone :: String | t } Phone
    phone = Phone <$> ((_.phone <$> Parser.stream) |= (not <<< Str.null) <?> "PhoneRequired")

message_ :: forall r. Parser { message :: String | r } (Maybe Message)
message_ = do
  msg <- _.message <$> Parser.stream
  if Str.null msg
    then pure Nothing
    else pure $ Just (Message msg)
