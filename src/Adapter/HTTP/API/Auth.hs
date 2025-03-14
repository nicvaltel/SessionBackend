{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.HTTP.API.Auth (routes) where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import Adapter.HTTP.Common
import Adapter.HTTP.API.Common
import Katip
import Data.Aeson -- ( object, KeyValue((.=)), Value (..), Result (..), fromJSON )
import Data.Aeson.Types (parseMaybe)
import Web.Cookie (SetCookie(..), def, sameSiteLax)
import Data.Time.Lens


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => ScottyT m ()
routes = do
  
  -- login 
  -- test : curl -i -d '{"username":"hello@mail.md", "password":"123456Hello"}' -H "Content-Type: application/json" -X POST http://localhost:3000/api/login
  post "/api/login" $ do
    inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
    case inputJson of
      Null -> pure () -- not a json
      mayCredentials -> 
        case isValidCredentials mayCredentials of
          Nothing -> pure () -- json is not an Auth
          Just (uname, pass) -> do
            print "AAAAA!!!!111"
            print (uname, pass)
            authResult <- lift $ findUserByEmailAndPassword uname pass
            case authResult of
              Nothing -> pure () -- username not found
              Just (_, False) -> pure () -- email not verified
              Just (uId, True) -> do -- Okay
                curTime <- liftIO getCurrentTime
                setCookie  def{
                      setCookieName = "session_token"
                    , setCookiePath = Just "/"
                    , setCookieValue = "randomly-generated-session-id"
                    , setCookieExpires = Just $ modL month (+ 1) curTime
                    , setCookieHttpOnly = True
                    , setCookieSecure = True
                    , setCookieSameSite = Just sameSiteLax
                    }
                json $ jsonResponce [ ("message" , "Login successful"), ("session_token", "randomly-generated-session-id")]

    print inputJson
    

    -- domainResult <- lift $ login input
    -- case domainResult of
    --   Left err -> do
    --     status status400
    --     json err
    --   Right sId -> do
    --     setSessionIdInCookie sId
    --     pure ()


  -- register
  post "/api/auth/register" $ do
    pure ()

  -- get user
  get "/api/users" $ do
    pure ()




-- Function to check if a JSON Value matches the expected structure
isValidCredentials :: Value -> Maybe (Text,Text)
isValidCredentials value = credentialsParser value >>= Just
  where  
      credentialsParser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          uname :: Text <- obj .: "username"
          pass :: Text <- obj .: "password"
          pure (uname, pass))