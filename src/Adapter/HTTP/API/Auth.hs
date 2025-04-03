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
import Domain.Room (RoomRepo(..), UserHost (..), LobbyRoomId (..), UserGuest (..), JoinRoomError (..), RoomId (..), GameParams(..))
import qualified Network.WebSockets.Connection as WS
import Adapter.WebSocket.WebSocketServer (wsSendGuestJoinedGameRoom)


type EndPointMonad m = (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m, RoomRepo m)


routes :: (EndPointMonad m, EmailVerificationNotif m) => ScottyT m ()
routes = do
  
  -- curl -i -d '{"username":"hello@mail.md", "password":"123456Hello"}' -H "Content-Type: application/json" -X POST http://localhost:3000/api/login
  post "/api/login" (postLogin "post /api/login")

  get "/api/logout" (getLogout "get /api/logout" )

  -- curl -i -v --cookie "session_id=sId" -X GET http://localhost:3000/api/session
  get "/api/session" (getSession "get /api/session")

  -- curl -i -v --cookie "session_id=sId" -X POST http://localhost:3000/api/create-room
  post "/api/create-room" (postCreateRoom "post /api/create-room")

  -- curl -i -v --cookie "session_id=sId" -X GET http://localhost:3000/api/lobby
  get "/api/lobby" (getLobby "get /api/lobby")

  -- curl -i -v --cookie "session_id=sId" -X GET http://localhost:3000/api/join-room/:room_id
  get "/api/join-room/:room_id" (getJoinRoom "/api/join-room/")

  -- register
  post "/api/register" (postRegister "post /api/register")

  get "/api/verify/:v_code" (getVerify "get /api/verify")

  get "/api/check-room/:room_id" (getCheckRoom "/api/check-room/")
  
  
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
    credentials -> 
      case extractCredentials credentials of
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
            Right (sessionId, userId) -> do
              -- log InfoS in loginViaEmailAndPassword function
              setCookieDefault "session_id" (encodeUtf8 sessionId) True
              json $ jsonResponce [("message" , "login successful"), ("user_id", tshow userId)]


getLogout :: EndPointMonad m => Namespace -> ActionT m ()
getLogout namespace = do
  let logAction = logAction' namespace
  maySession <- checkSessionActionT logAction
  case maySession of
    Nothing -> pure ()
    Just (_, sId, _) -> do
      -- log InfoS in logout function      
      deleteCookieDefault "session_id" True
      lift $ logout sId
      json $ object ["message" .= ("logout" :: Text)]



getSession :: EndPointMonad m => Namespace -> ActionT m ()
getSession namespace = do
  let logAction = logAction' namespace
  maySession <- checkSessionActionT logAction
  case maySession of
    Nothing -> pure ()
    Just (uId, sId, _) -> do
      logAction InfoS $ ls $ "session is active " <> sId
      json $ jsonResponce [("message", "session active"), ("user_id", tshow uId)]


postCreateRoom :: EndPointMonad m => Namespace -> ActionT m ()
postCreateRoom namespace = do
  let logAction = logAction' namespace
  inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
  case inputJson of
    Null -> do -- not a json
      logAction WarningS "no json provided"
      json $ jsonResponce [("error" , "no json provided")]
    gameParamsValue -> 
      case extractGameParams gameParamsValue of
        Nothing -> do
          logAction WarningS "json is not a game data"
          json $ jsonResponce [("error" , "json is not a game data")]
        Just gameParams -> do
          maySession <- checkSessionActionT logAction
          case maySession of
            Nothing -> pure ()
            Just (hostId, sId, Nothing) -> do
              logAction WarningS $ ls $ "attempt to create room without active websocket connection userId=" <> tshow hostId <> ", sessionId=" <> sId
              json $ jsonResponce [("error", "websocket connection is not active")]
            Just (hostId, sId, Just hostConn) -> do
              (LobbyRoomId roomId) <- lift $ createRoom UserHost{hostId , hostConn}
              logAction InfoS $ ls $ "room created by userId=" <> tshow hostId <> ", sessionId=" <> sId <> ", lobbyRoomId =" <> roomId
              json $ jsonResponce [("lobby_room_id", roomId)]


getLobby :: EndPointMonad m => Namespace -> ActionT m ()
getLobby namespace = do
  let logAction = logAction' namespace
  maySession <- checkSessionActionT logAction
  case maySession of
    Nothing -> pure ()
    Just _ -> do
      lobby <- lift getOpenRooms
      json $ object ["lobby_rooms_id" .= map unLobbyRoomId lobby]


getJoinRoom :: EndPointMonad m => Text -> ActionT m ()
getJoinRoom namespace = do
  roomId :: Text <- pathParam "room_id"
  let logAction = logAction' (Namespace [namespace <> roomId])
  maySession <- checkSessionActionT logAction
  case maySession of
    Nothing -> pure ()
    Just (guestId, sId, Nothing) -> do
      logAction WarningS $ ls $ "attempt to join room without active websocket connection userId=" <> tshow guestId <> ", sessionId=" <> sId
      json $ jsonResponce [("error", "websocket connection is not active")]
    Just (guestId, _, Just guestConn) -> do
      eitherRoomId <- lift $ joinRoom UserGuest{guestId, guestConn} (LobbyRoomId roomId)
      case eitherRoomId of
        Left JoinRoomErrorRoomDoesntExist -> do
          logAction InfoS "room is not active"
          json $ object ["error" .= ("room is not active" :: Text)]
        Right (RoomId rId, host) -> do
          liftIO $ wsSendGuestJoinedGameRoom (hostConn host) (RoomId rId)
          logAction InfoS ("room started, roomId=" <> ls rId)
          json $ object ["room_id" .= rId]


postRegister :: (EndPointMonad m, EmailVerificationNotif m) => Namespace -> ActionT m ()
postRegister namespace = do
  let logAction = logAction' namespace
  inputJson :: Value <- jsonData `catch` (\(_:: SomeException) -> pure Null)
  case inputJson of
    Null -> do -- not a json
      logAction WarningS "no json provided"
      json $ jsonResponce [("error" , "no json provided")]
    registerData -> 
      case extractRegisterData registerData of
        Nothing -> do -- json is not an Auth
          logAction WarningS "json is not a register data"
          json $ jsonResponce [("error" , "json is not a register data")]
        Just (email, pass) -> do
          eitherRegError <- liftKatipContext $ registerViaEmailPassword email pass
          case eitherRegError of
            Left RegistrationErrorEmailTaken -> do
              logAction InfoS ("registration error email taken, email=" <> ls email)
              json $ object ["error" .= ("registration error email taken" :: Text)]
            Left (RegistrationErrorIncorrectEmailOrPassword errs) -> do
              logAction InfoS ("incorrect email or password, email=" <> ls email)
              json $ object ["error" .= ("incorrect email or password" :: Text), "error_messages" .= errs]
            Right () -> do
              logAction InfoS ("user registered, email=" <> ls email)
              json $ object ["message" .= ("verification code sended" :: Text)]

getVerify :: EndPointMonad m => Text -> ActionT m ()
getVerify namespace = do
  vCode :: Text <- pathParam "v_code"
  let logAction = logAction' (Namespace [namespace <> vCode])
  eitherVerifError <- liftKatipContext $ verifyEmail vCode
  case eitherVerifError of
    Left EmailVerificationErrorInvalidCode -> do
      logAction InfoS "email verification error invalid code"
      json $ object ["error" .= ("email verification error invalid code" :: Text)]
    Right () -> do
      logAction InfoS $ "email verification successful, vcode =" <> ls vCode
      json $ object ["message" .= ("email verification successful" :: Text)]
      

getCheckRoom :: EndPointMonad m => Text -> ActionT m ()
getCheckRoom namespace = do
  roomId :: Text <- pathParam "room_id"
  let logAction = logAction' (Namespace [namespace <> roomId])
  mayRoomId <- lift $ checkLobbyRoomIsGameStarted (LobbyRoomId roomId)
  case mayRoomId of
    Nothing -> json $ object ["message" .= ("" :: Text)]
    Just (RoomId rId) -> do
      logAction InfoS ("roomId of started room sended to host, roomId=" <> ls rId)
      json $ object ["room_id" .= rId]


checkSessionActionT :: (MonadIO m, SessionRepo m) => (Severity -> LogStr -> ActionT m ()) -> ActionT m (Maybe (UserId, SessionId, Maybe WS.Connection))
checkSessionActionT logAction = do
  maySessionId <- getCookie "session_id"
  case maySessionId of
    Nothing -> do
      logAction InfoS "session is not active"
      json $ jsonResponce [("error", "unauthorized"), ("delete_local_storage", "user_id")]
      pure Nothing
    Just sId -> do
      mayUserIdConn <- lift $ findUserIdBySessionId sId
      case mayUserIdConn of
        Nothing -> do
          deleteCookieDefault "session_id" True
          logAction InfoS "session is not active "
          json $ jsonResponce [("error", "unauthorized"), ("delete_local_storage", "user_id")]
          pure Nothing
        Just (uId, mayConn) -> pure (Just (uId, sId, mayConn))

        
extractCredentials :: Value -> Maybe (Text,Text)
extractCredentials value = credentialsParser value >>= Just
  where  
      credentialsParser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          uname :: Text <- obj .: "username"
          pass :: Text <- obj .: "password"
          pure (uname, pass))


extractRegisterData :: Value -> Maybe (Text, Text)
extractRegisterData value = credentialsParser value >>= Just
  where  
      credentialsParser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          email :: Text <- obj .: "email"
          pass :: Text <- obj .: "password"
          pure (email, pass))

extractGameParams :: Value -> Maybe GameParams
extractGameParams value = parser value >>= getGameParams
  where  
      parser = 
        parseMaybe (withObject "Credentials" $ \obj -> do
          gameType :: Text <- obj .: "game_type"
          pure gameType)

      getGameParams :: Text -> Maybe GameParams
      getGameParams "rated" = Just GameParamsRated
      getGameParams "casual" = Just GameParamsCasual
      getGameParams _ = Nothing

-- Function to extract sessionId from a JSON Value
extractSessionId :: Value -> Maybe SessionId
extractSessionId = parseMaybe (withObject "Session" $ \obj -> obj .: "session_id")
