module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.String (length)
import Global.Unsafe (unsafeStringify)

data Validation err a = Failure err | Success a

instance showV :: (Show err, Show a) => Show (Validation err a) where
  show (Failure err) = "(Invalid " <> show err <> ")"
  show (Success a)   = "(Valid "   <> show a   <> ")"

instance functorV :: Functor (Validation err) where
  map f (Success a) = Success (f a)
  map _ (Failure e) = Failure e

instance semigroupV :: Semigroup err => Apply (Validation err) where
  apply (Success f) (Success a)  = Success (f a)
  apply (Success _) (Failure e)  = Failure e
  apply (Failure e) (Success _)  = Failure e
  apply (Failure e) (Failure e') = Failure (e <> e')

newtype Form
  = Form
  { email    :: String
  , password :: String
  }

data Error
  = EmptyField
  | NotMinLength

instance showError :: Show Error where
  show EmptyField   = "EmptyField"
  show NotMinLength = "NotMinLength"

newtype Email = Email String

newtype Password = Password String

data ValidatedForm = ValidatedForm Email Password

type FormValidation a = Validation (NonEmptyList Error) a

notEmpty :: String -> FormValidation String
notEmpty ""  = Failure (singleton EmptyField)
notEmpty str = Success str

minLength :: String -> Int -> FormValidation String
minLength str n
  | length str >= n = Success str
  | otherwise       = Failure (singleton NotMinLength)

minPasswordLength :: Int
minPasswordLength = 8

validateEmail :: String -> FormValidation Email
validateEmail input =
  notEmpty input $>
  Email input

validatePassword :: String -> FormValidation Password
validatePassword input =
  notEmpty input *>
  minLength input minPasswordLength $>
  Password input

validateForm :: Form -> FormValidation ValidatedForm
validateForm (Form {email, password}) =
  ValidatedForm
  <$> validateEmail email
  <*> validatePassword password

mkForm :: String -> String -> FormValidation ValidatedForm
mkForm email password = validateForm $ Form {email, password}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let form = mkForm "steve@email.com" "12345678"
  logShow (map unsafeStringify form)
