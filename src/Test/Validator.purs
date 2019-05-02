module Goat.Validator where

import Prelude                     (class Ord, Unit, map, not, pure, unit, ($), (*>), (<$>), (<*>), (<<<), (<=), (>=), (>>>))
import Data.Either                 (Either)
import Data.Either               as Either
import Data.Int                  as Int
import Data.Maybe                  (Maybe(..))
import Data.String               as Str
import Data.Validation.Semigroup   (V(..))
import Data.Validation.Semigroup as V
import Test.Types                  (Age(..), Contact(..), ContactType(..), Email(..), Goat, Horns(..), Message(..), Name(..), Phone(..), RegisterForm)


toGoat :: RegisterForm -> Either (Array String) Goat
toGoat form =
  V.toEither $ { name: _, age: _, horns: _, contact: _, message: _ }
    <$> name_ form
    <*> age_ form
    <*> horns_ form
    <*> contact_ form
    <*> message_ form

name_ :: forall r.  { name :: String | r } -> V (Array String) Name
name_ = _.name
  >>> (\n -> required "NameRequired" n *> pure n)
  >>> map Name

age_ :: forall r. { age :: String | r } -> V (Array String) Age
age_ =
  _.age >>>
  (\n -> required "AgeRequired" n
    `V.andThen`
    (\_ -> int "AgeInvalidInt" n)
    `V.andThen`
    (\m ->
      minBound "AgeNegative" 0 m *>
      pure m
    )
  ) >>>
  map Age

horns_ :: forall r. { horns :: String | r } -> V (Array String) Horns
horns_ =
  _.horns >>>
  (\n -> required "HornsRequired" n
    `V.andThen`
    (\_ -> int "HornsInvalidInt" n)
    `V.andThen`
    (\m ->
      minBound "HornsNegative" 0 m *>
      maxBound "HornsTooMany" 2 m *>
      pure m
    )
  ) >>>
  map Horns

contact_ :: forall r. { contactType :: String, email :: String, phone :: String | r } -> V (Array String) Contact
contact_ form =
  (required "ContactTypeRequired" form.contactType *> pure form.contactType)
  `V.andThen`
  (\cTypeStr -> case cTypeStr of
    "ContactEmail" -> pure UseEmail
    "ContactPhone" -> pure UsePhone
    _              -> V.invalid ["ContactTypeInvalid"]
  )
  `V.andThen`
  (\ctype -> case ctype of
    UseEmail -> ContactEmail <$> email form.email
    UsePhone -> ContactPhone <$> phone form.phone
  )
  where
    email :: String -> V (Array String) Email
    email str = required "EmailRequired" str *> pure (Email str)
    phone :: String -> V (Array String) Phone
    phone str = required "PhoneRequired" str *> pure (Phone str)

message_ :: forall r. { message :: String | r } -> V (Array String) (Maybe Message)
message_ =
  _.message >>>
  (\msg -> if Str.null msg
    then pure Nothing
    else pure $ Just (Message msg)
  )

chk :: forall e v. e -> (v -> Boolean) -> v -> V (Array e) Unit
chk e r v = if r v
    then pure unit
    else V.invalid [e]

required :: forall e. e -> String -> V (Array e) Unit
required err = chk err (not <<< Str.null)

int :: forall e. e -> String -> V (Array e) Int
int e = V <<< Either.note [ e ] <<< Int.fromString

minBound :: forall e a. Ord a => e -> a -> a -> V (Array e) Unit
minBound err bound = chk err (_ >= bound)

maxBound :: forall e a. Ord a => e -> a -> a -> V (Array e) Unit
maxBound err bound = chk err (_ <= bound)
