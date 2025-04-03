{-# LANGUAGE QuasiQuotes #-}

module Domain.Auth
  ( Auth(..)
  , Email
  , emailRaw
  , Password
  , RegistrationError(..)
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)
  , EmailVerificationError(..)
  , LoginError(..)
  , SessionError(..)
  , VerificationCode
  , SessionId
  , UserId
  , getUser
  , login
  , verifyEmail
  , register
  , mkEmail
  , mkPassword
  , loginViaEmailAndPassword
  , logout
  , registerViaEmailPassword
  , resolveSessionId
  , mkUnsafePassword
  , mkEmailUnsafe
  ) where

import ClassyPrelude
import Text.Regex.PCRE.Heavy
import Domain.Validation
import Control.Monad.Except (runExceptT, ExceptT (ExceptT), MonadError (throwError))
import Katip
import qualified Network.WebSockets as WS

type VerificationCode = Text

type UserId = Int

type SessionId = Text

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

newtype Password = Password { passwordRaw :: Text} 
  deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken | RegistrationErrorIncorrectEmailOrPassword [Text]
  deriving (Show, Eq)

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError =  LoginErrorInvalidAuth | LoginErrorEmailNotVerified
  deriving (Show, Eq)

data SessionError = SessionErrorSessionIsNotActive
  deriving (Show, Eq)

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)


mkEmail :: Text -> Either [Text] Email
mkEmail emailRaw = Right Email{emailRaw} -- Check email via verification only, without regexp check

mkEmailUnsafe :: Text -> Email
mkEmailUnsafe = Email

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [ lengthBetween 10 50 "Should between 10 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

mkUnsafePassword :: Text -> Password
mkUnsafePassword = Password


class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool)) -- Bool = email is verified
  findEmailFromUserId :: UserId -> m (Maybe Email)


class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  addWSConnection :: SessionId -> WS.Connection -> m (Either SessionError ())
  endSession :: SessionId -> m ()
  findUserIdBySessionId :: SessionId -> m (Maybe (UserId, Maybe WS.Connection))

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
  
registerViaEmailPassword :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Text -> Text -> m (Either RegistrationError ())
registerViaEmailPassword emailText passText = do
  let eitherAuth = do
        authEmail <- mkEmail emailText
        authPassword <- mkPassword passText
        pure Auth{authEmail, authPassword}
  case eitherAuth of
    Left errors -> pure $ Left (RegistrationErrorIncorrectEmailOrPassword errors)
    Right auth -> register auth
 
verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (emailRaw email) <> " is verified successfully"


login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError (SessionId, UserId))
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, True) -> do
      sId <- lift $ newSession uId
      withUserIdContext uId $
        $(logTM) InfoS $ ls (emailRaw $ authEmail auth) <> " logged in successfully"
      pure (sId, uId)

loginViaEmailAndPassword :: (KatipContext m, AuthRepo m, SessionRepo m) => Text -> Text -> m (Either LoginError (SessionId, UserId))
loginViaEmailAndPassword email password = login Auth{authEmail = Email email, authPassword = Password password}

logout :: (KatipContext m, SessionRepo m) => SessionId -> m ()
logout sessionId = do
      katipAddContext (sl "sessionId" sessionId) $
        $(logTM) InfoS "session is logged out"
      endSession sessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId sId = (fst <$>) <$> findUserIdBySessionId sId
