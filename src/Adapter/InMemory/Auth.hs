{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Prelude
import qualified Domain.Auth as D
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Domain.Auth (Auth(authEmail))
import Data.Has (Has (..))
import Control.Monad.Except ( runExceptT, MonadError(throwError) )
import Text.StringRandom (stringRandomIO)
import qualified Domain.Room as D
import Adapter.Email.SendMail (sendMail)
import qualified Network.WebSockets as WS



type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State 
  { stateAuth :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId (D.UserId, Maybe WS.Connection)
  , stateLobbyRooms :: [(D.LobbyRoomId, D.UserHost)]
  , stateRooms :: Map D.RoomId D.RoomData
  , stateUsersRooms :: Map D.UserId D.RoomId
  , stateArchiveRooms :: Map D.ArchiveRoomId D.RoomData
  }


initialState :: State
initialState = State
  { stateAuth = []
  , stateUnverifiedEmails = Map.empty
  , stateVerifiedEmails = Set.empty
  , stateUserIdCounter = 0
  , stateNotifications = Map.empty
  , stateSessions = Map.empty
  , stateLobbyRooms = []
  , stateRooms = Map.empty
  , stateUsersRooms = Map.empty
  , stateArchiveRooms = Map.empty
  }


findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar 
  pure $ fst <$> Map.lookup sId (stateSessions state)
 

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter 
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  liftIO $ atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = Map.insert sId (uId, Nothing) sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    pure sId

addWSConnection :: InMemory r m => D.SessionId -> WS.Connection -> m ()
addWSConnection sId wsConn = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = Map.adjust (\(uId,_) -> (uId, Just wsConn)) sId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState

endSession :: InMemory r m => D.SessionId -> m ()
endSession sId = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = Map.delete sId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    pure ()

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  liftIO $ atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = Map.insert email vCode notifications
        newState = state {stateNotifications = newNotifications}
    writeTVar tvar newState
  sendMail email "Email Verification Code" ("Email Verification Code=" <> vCode)

getNotificationsForEmail ::  InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ Map.lookup email (stateNotifications state)


findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ D.authEmail <$> lookup uId (stateAuth state)


findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool)) -- Bool = email is verified
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = fst <$> find ((auth ==) . snd) (stateAuth state)
  case mayUserId of
    Nothing -> pure Nothing
    Just uId -> do
      let isVerified = D.authEmail auth `Set.member` stateVerifiedEmails state 
      pure $ Just (uId, isVerified)


orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = pure a

setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    let mayEmail = Map.lookup vCode (stateUnverifiedEmails state)
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let mayUserId = fst <$> find ((email ==) . D.authEmail . snd) (stateAuth state)
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let newState = state 
          { stateUnverifiedEmails = Map.delete vCode (stateUnverifiedEmails state)
          , stateVerifiedEmails = Set.insert email (stateVerifiedEmails state)
          }
    lift $ writeTVar tvar newState
    pure (uId, email)

addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}" 
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    let email = D.authEmail auth
    let emailAlreadyTaken = email `elem` [ authEmail | (_, D.Auth{authEmail}) <- stateAuth state]
    when emailAlreadyTaken $ throwError D.RegistrationErrorEmailTaken
    let newUserId = stateUserIdCounter state + 1
    let newState = state
          { stateAuth = (newUserId, auth) : stateAuth state
          , stateUnverifiedEmails = Map.insert vCode email (stateUnverifiedEmails state)
          , stateUserIdCounter = newUserId
          }
    lift $ writeTVar tvar newState
    pure (newUserId, vCode)


createRoom :: InMemory r m => D.UserHost -> m D.LobbyRoomId
createRoom hostId = do
  tvar <- asks getter
  roomId <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
  let newRoomInList = (D.LobbyRoomId roomId, hostId)
  liftIO $ atomically $ do
    state <- readTVar tvar
    let newLobbyRooms = newRoomInList : stateLobbyRooms state
    writeTVar tvar $ state {stateLobbyRooms = newLobbyRooms}
  pure (D.LobbyRoomId roomId)


getOpenRooms :: InMemory r m => m [D.LobbyRoomId]
getOpenRooms = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ map fst $ stateLobbyRooms state


joinRoom :: InMemory r m => D.UserGuest -> D.LobbyRoomId -> m (Either D.JoinRoomError D.RoomId)
joinRoom guestId lobbyRoomId = do
  tvar <- asks getter
  let roomId = D.lobbyRoomIdToRoomId lobbyRoomId
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    case Prelude.lookup lobbyRoomId (stateLobbyRooms state) of
      Nothing -> throwError D.JoinRoomErrorRoomDoesntExist
      Just hostId -> do
        let roomData = D.newRoomData hostId guestId
        let newLobbyRooms = filter (\(lrId,_) -> lobbyRoomId /= lrId) (stateLobbyRooms state)
        let newRooms = Map.insert roomId roomData(stateRooms state)
        let newUsersRooms = 
              Map.insert (D.unUserHost hostId) roomId $ 
              Map.insert (D.unUserGuest guestId) roomId $
              stateUsersRooms state
        lift $ writeTVar tvar state{stateLobbyRooms = newLobbyRooms, stateRooms = newRooms, stateUsersRooms = newUsersRooms}
        pure roomId 


closeRoom :: InMemory r m => D.RoomId -> m (Either D.CloseRoomError D.ArchiveRoomId)
closeRoom roomId = do
  tvar <- asks getter
  let acrhiveRoomId = D.roomIdToAcrhiveRoomId roomId
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    case Map.lookup roomId (stateRooms state) of
      Nothing -> throwError D.CloseRoomErrorRoomDoesntExist
      Just roomData -> do
        let newRooms = Map.delete roomId (stateRooms state)
        let newArchiveRooms = 
              Map.insert acrhiveRoomId roomData $ stateArchiveRooms state
        lift $ writeTVar tvar state{stateRooms = newRooms, stateArchiveRooms = newArchiveRooms}
        pure acrhiveRoomId 


checkLobbyRoomIsGameStarted :: InMemory r m => D.LobbyRoomId -> m (Maybe D.RoomId)
checkLobbyRoomIsGameStarted (D.LobbyRoomId rId) = do
  let roomId = D.RoomId rId
  tvar <- asks getter
  liftIO $ atomically $ do
    state <- readTVar tvar
    case Map.lookup roomId (stateRooms state) of
      Nothing -> pure Nothing
      Just _ -> pure (Just roomId)


--------------------------------------------------------------

runTest :: IO ()
runTest = do
  let email = D.mkEmail "hello@test.com"
  let passw = D.mkPassword "12345ASDFqwer"
  let auth = either undefined id $ D.Auth <$> email <*> passw 
  s <- newTVarIO initialState 
  
  res0 <- flip runReaderT s $ do
      vCode <- addAuth auth
      user <- findUserByAuth auth
      email' <- findEmailFromUserId 1
      sId <- newSession 1
      uId <- findUserIdBySessionId sId
      pure (vCode, user, email', sId, uId)
  print res0
  pure ()
