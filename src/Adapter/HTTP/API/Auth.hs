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
import Web.Scotty.Cookie (deleteCookie)


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m) => ScottyT m ()
routes = do
  
  -- curl -i -d '{"username":"hello@mail.md", "password":"123456Hello"}' -H "Content-Type: application/json" -X POST http://localhost:3000/api/login
  post "/api/login" $ do
    let logAction severity message = lift . katipAddNamespace "post /api/login" $ $(logTM) severity message
    inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
    case inputJson of
      Null -> do -- not a json
        logAction WarningS "no json provided"
        json $ jsonResponce [("error" , "no json provided")]
      mayCredentials -> 
        case extrectCredentials mayCredentials of
          Nothing -> do -- json is not an Auth
            logAction WarningS "json is not an auth"
            json $ jsonResponce [("error" , "json is not an auth")]
          Just (uname, pass) -> do
            loginResult <- lift $ loginViaEmailAndPassword uname pass
            case loginResult of
              Left LoginErrorInvalidAuth -> do 
                logAction WarningS ("Invalid credentials for: " <> ls uname)
                json $ jsonResponce [("error" , "invalid credentials")]
              Left LoginErrorEmailNotVerified -> do
                logAction WarningS ("email not verified: " <> ls uname)
                json $ jsonResponce [("error" , "email not verified")]
              Right sessionId -> do
                -- log InfoS in loginViaEmailAndPassword function
                setCookieDefault "session_token" (encodeUtf8 sessionId) True
                json $ jsonResponce [ ("message" , "login successful"), ("session_token", sessionId)]


  post "/api/logout" $ do
    let logAction severity message = lift . katipAddNamespace "post /api/logout" $ $(logTM) severity message
    inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
    case inputJson of
      Null -> do -- not a json
        logAction WarningS "no json provided"
        json $ jsonResponce [("error" , "no json provided")]
      maySessionId -> 
        case extractSessionId maySessionId of
          Nothing -> do -- json is not a sessionId
            logAction WarningS "json is not a sessionId"
            json $ jsonResponce [("error" , "json is not a sessionId")]
          Just sessionId -> do
            lift $ logout sessionId
            -- log InfoS in logout function
            deleteCookie "session_token"
            json $ jsonResponce [ ("message" , "logged out successfully")]


  -- curl -i -v --cookie "session_token=sId" -X GET http://localhost:3000/api/session
  get "/api/session" $ do
    let logAction severity message = lift . katipAddNamespace "get /api/session" $ $(logTM) severity message
    maySessionId <- getCookie "session_token"
    case maySessionId of
      Nothing -> do
        logAction InfoS "session is not active "
        json $ jsonResponce [("error", "session expired or invalid")]
      Just sId -> do
        mayUserId <- lift $ findUserIdBySessionId sId
        case mayUserId of
          Nothing -> do
            deleteCookie "session_token"
            logAction InfoS "session is not active "
            json $ jsonResponce [("error", "session expired or invalid")]
          Just uId -> do
            logAction InfoS $ ls $ "session is active " <> sId
            json $ jsonResponce [("message", "session active"), ("user_id", tshow uId)]


  -- curl -i -v --cookie "session_token=sId" -X GET http://localhost:3000/api/protected-resource
  get "/api/protected-resource" $ do
    let logAction severity message = lift . katipAddNamespace "get /api/protected-resource" $ $(logTM) severity message
    maySessionId <- getCookie "session_token"
    case maySessionId of
      Nothing -> do
        logAction InfoS "session is not active "
        json $ jsonResponce [("error", "unauthorized")]
      Just sId -> do
        mayUserId <- lift $ findUserIdBySessionId sId
        case mayUserId of
          Nothing -> do
            deleteCookie "session_token"
            logAction InfoS "session is not active "
            json $ jsonResponce [("error", "unauthorized")]
          Just _ -> do
            logAction InfoS $ ls $ "session is active " <> sId
            json $ jsonResponce [("data", "Here is your protected data")]

  -- register
  post "/api/auth/register" $ do
    pure ()

  -- get user
  get "/api/users" $ do
    pure ()


extrectCredentials :: Value -> Maybe (Text,Text)
extrectCredentials value = credentialsParser value >>= Just
  where  
      credentialsParser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          uname :: Text <- obj .: "username"
          pass :: Text <- obj .: "password"
          pure (uname, pass))


-- Function to extract sessionId from a JSON Value
extractSessionId :: Value -> Maybe SessionId
extractSessionId = parseMaybe (withObject "Session" $ \obj -> obj .: "sessionId")
