module Goat.Decoder where

import Prelude                     (bind, pure, ($), (*>), (<$>), (<*>), (<<<), (>>=), (>>>))
import Control.Alt                 ((<|>))
import Data.Either                 (Either)
import Data.Form.Decoder           (Decoder(..))
import Data.Form.Decoder         as Decoder
import Data.Maybe                  (Maybe(..))
import Data.String               as Str
import Data.Validation.Semigroup as V
import Test.Types                  (Age(..), Contact(..), ContactType(..), Email(..), Goat, Horns(..), Message(..), Name(..), Phone(..), RegisterForm)

toGoat :: RegisterForm -> Either (Array String) Goat
toGoat = V.toEither <<< (Decoder.run goat)

goat :: Decoder (Array String) RegisterForm Goat
goat = { name: _, age: _, horns: _, contact: _, message: _ }
  <$> name_
  <*> age_
  <*> horns_
  <*> contact_
  <*> message_

name_ :: forall r. Decoder (Array String) { name :: String | r } Name
name_ = Name <$> Decoder.hoist _.name (Decoder.required ["NameRequired"] *> Decoder.identity)

age_ :: forall r. Decoder (Array String) { age :: String | r } Age
age_ = Age <$> Decoder.hoist _.age age
  where
    age :: Decoder (Array String) String Int
    age = (Decoder.required ["AgeRequired"] >>= (\_ -> Decoder.int ["AgeInvalidInt"]))
          >>> (
            Decoder.minBound ["AgeNegative"] 0 *>
            Decoder.identity
          )

horns_ :: forall r. Decoder (Array String) { horns :: String | r } Horns
horns_ = Horns <$> Decoder.hoist _.horns horns
  where
    horns :: Decoder (Array String) String Int
    horns = (Decoder.required ["HornsRequired"] >>= (\_ -> Decoder.int ["HornsInvalidInt"]))
          >>> (
            Decoder.minBound ["HornsNegative"] 0 *>
            Decoder.maxBound ["HornsTooMany"]  2 *>
            Decoder.identity
          )

contact_ :: forall r. Decoder (Array String) { contactType :: String, email :: String, phone :: String | r } Contact
contact_ = do
  ctype <- (contactType UseEmail "ContactEmail") <|> (contactType UsePhone "ContactPhone")
  case ctype of
    UseEmail -> ContactEmail <$> email
    UsePhone -> ContactPhone <$> phone
  where
    contactType :: forall s. ContactType -> String -> Decoder (Array String) { contactType :: String | s } ContactType
    contactType s t = Decoder.hoist _.contactType $
        (Decoder.required ["ContactTypeRequired"] >>= \_ -> Decoder.symbol ["ContactTypeInvalid"] s t)
    email :: forall t. Decoder (Array String) { email :: String | t } Email
    email = Email <$> Decoder.hoist _.email (Decoder.required ["EmailRequired"] *> Decoder.identity)
    phone :: forall u. Decoder (Array String) { phone :: String | u } Phone
    phone = Phone <$> Decoder.hoist _.phone (Decoder.required ["PhoneRequired"] *> Decoder.identity)

message_ :: forall r. Decoder (Array String) { message :: String | r } (Maybe Message)
message_ = Decoder.hoist _.message $
  Decoder \msg -> if Str.null msg
    then pure Nothing
    else pure $ Just (Message msg)
