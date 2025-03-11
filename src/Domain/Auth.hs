{-# LANGUAGE QuasiQuotes #-}

module Domain.Auth
  ( Auth(..)
  , Email
  , Password
  , RegistrationError(..)
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)
  , EmailVerificationError(..)
  , LoginError(..)
  , VerificationCode
  , SessionId
  , UserId
  , getUser
  , resolveSessionId
  , login
  , verifyEmail
  , register
  , mkEmail
  , mkPassword
  ) where

import ClassyPrelude
import Text.Regex.PCRE.Heavy
import Domain.Validation
import Control.Monad.Except (runExceptT, ExceptT (ExceptT), MonadError (throwError))
import Katip

type VerificationCode = Text

type UserId = Int

type SessionId = Text

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

newtype Password = Password { passwordRaw :: Text} 
  deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError =  LoginErrorInvalidAuth | LoginErrorEmailNotVerified
  deriving (Show, Eq)

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)


mkEmail :: Text -> Either [Text] Email
mkEmail emailRaw = Right Email{emailRaw} -- Check email via verification only, without regexp check


mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [ lengthBetween 10 50 "Should between 10 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]



class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool)) -- Bool = email is verified
  findEmailFromUserId :: UserId -> m (Maybe Email)


class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()



withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (emailRaw email) <> " is registered successfully"
  
  
 
verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (emailRaw email) <> " is verified successfully"


login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, True) -> do
      sId <- lift $ newSession uId
      withUserIdContext uId $
        $(logTM) InfoS $ ls (emailRaw $ authEmail auth) <> " logged in successfully"
      pure sId


getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId