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


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m) => ScottyT m ()
routes = do
  
  -- login 
  -- test : curl -i -d '{"username":"hello@mail.md", "password":"123456Hello"}' -H "Content-Type: application/json" -X POST http://localhost:3000/api/login
  post "/api/login" $ do
    inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
    case inputJson of
      Null -> do -- not a json
        lift $ katipAddNamespace "post /api/login" $
          $(logTM) WarningS "no json provided"
        json $ jsonResponce [("error" , "no json provided")]
      mayCredentials -> 
        case isValidCredentials mayCredentials of
          Nothing -> do -- json is not an Auth
            lift $ katipAddNamespace "post /api/login" $
              $(logTM) WarningS "json is not an auth"
            json $ jsonResponce [("error" , "json is not an auth")]
          Just (uname, pass) -> do
            print (uname, pass)
            authResult <- lift $ findUserByEmailAndPassword uname pass
            case authResult of
              Nothing -> do -- username not found
                lift $ katipAddNamespace "post /api/login" $
                  $(logTM) WarningS ("username not found for: " <> ls uname)
                json $ jsonResponce [("error" , "username not found")]
              Just (_, False) -> do -- email not verified
                lift $ katipAddNamespace "post /api/login" $
                  $(logTM) WarningS ("email not verified: " <> ls uname)
                json $ jsonResponce [("error" , "email not verified")]
              Just (uId, True) -> do -- Okay
                lift $ katipAddNamespace "post /api/login" $
                  $(logTM) InfoS ("login successfully: " <> ls uname)
                sessionId <- lift $ newSession uId
                setCookieDefault "session_token" (encodeUtf8 sessionId) True
                json $ jsonResponce [ ("message" , "Login successful"), ("session_token", sessionId)]
    

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



isValidCredentials :: Value -> Maybe (Text,Text)
isValidCredentials value = credentialsParser value >>= Just
  where  
      credentialsParser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          uname :: Text <- obj .: "username"
          pass :: Text <- obj .: "password"
          pure (uname, pass))