{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Domain.Room (RoomRepo(..), UserHost (..), LobbyRoomId (..), UserGuest (UserGuest), JoinRoomError (..), RoomId (..))


type EndPointMonad m = (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m, RoomRepo m)


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m, RoomRepo m) => ScottyT m ()
routes = do
  
  -- curl -i -d '{"username":"hello@mail.md", "password":"123456Hello"}' -H "Content-Type: application/json" -X POST http://localhost:3000/api/login
  post "/api/login" (postLogin "post /api/login")

  post "/api/logout" (postLogout "post /api/logout" )

  -- curl -i -v --cookie "session_token=sId" -X GET http://localhost:3000/api/session
  get "/api/session" (getSession "get /api/session")

  -- curl -i -v --cookie "session_token=sId" -X POST http://localhost:3000/api/create-room
  post "/api/create-room" (postCreateRoom "post /api/create-room")

  -- curl -i -v --cookie "session_token=sId" -X GET http://localhost:3000/api/lobby
  get "/api/lobby" (getLobby "get /api/lobby")

  -- curl -i -v --cookie "session_token=sId" -X GET http://localhost:3000/api/join-room/:room_id
  get "/api/join-room/:room_id" (getJoinRoom "/api/join-room/")

  -- register
  post "/api/auth/register" $ do
    pure ()

  -- get user
  get "/api/users" $ do
    pure ()


logAction' :: (MonadUnliftIO m, KatipContext m) => Namespace -> Severity -> LogStr -> ActionT m ()
logAction' namespace severity message = lift . katipAddNamespace namespace $ $(logTM) severity message


postLogin :: EndPointMonad m => Namespace -> ActionT m ()
postLogin namespace = do
  let logAction = logAction' namespace
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

postLogout :: EndPointMonad m => Namespace -> ActionT m ()
postLogout namespace = do
  let logAction = logAction' namespace
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

getSession :: EndPointMonad m => Namespace -> ActionT m ()
getSession namespace = do
  let logAction = logAction' namespace
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


postCreateRoom :: EndPointMonad m => Namespace -> ActionT m ()
postCreateRoom namespace = do
  let logAction = logAction' namespace
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
        Just uId -> do
          (LobbyRoomId roomId) <- lift $ createRoom (UserHost uId)
          logAction InfoS $ ls $ "room created by userId=" <> tshow uId <> ", sessionId=" <> sId <> ", lobbyRoomId =" <> roomId
          json $ jsonResponce [("lobby_room_id", roomId)]

getLobby :: EndPointMonad m => Namespace -> ActionT m ()
getLobby namespace = do
  let logAction = logAction' namespace
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
          lobby <- lift getOpenRooms
          json $ object ["lobby_rooms_id" .= map unLobbyRoomId lobby]


getJoinRoom :: EndPointMonad m => Text -> ActionT m ()
getJoinRoom namespace = do
  roomId :: Text <- pathParam "room_id"
  let logAction = logAction' (Namespace [namespace <> roomId])
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
        Just uId -> do
          eitherRoomId <- lift $ joinRoom (UserGuest uId) (LobbyRoomId roomId)
          case eitherRoomId of
            Left JoinRoomErrorRoomDoesntExist -> do
              logAction InfoS "room is not active"
              json $ jsonResponce [("error", "room is not active")]
            Right (RoomId rId) -> do
              logAction InfoS ("room started, roomId=" <> ls rId)
              json $ object ["room_id" .= rId]

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
