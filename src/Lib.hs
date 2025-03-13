{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib (runRoutine) where

import ClassyPrelude
import Domain.Auth
import qualified Adapter.InMemory.Auth as Mem
import qualified Configuration.Dotenv as Dotenv
import qualified Data.Text as T

import Katip (KatipContextT, LogEnv, KatipContext, Katip, runKatipContextT)
import Logging (withKatip)
import qualified Data.ByteString.Char8 as BSC8
import Control.Monad (MonadFail)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import qualified Prelude
import qualified Adapter.HTTP.Main as HTTP



-- type AppState = (PG.State, RDS.State, MQ.State, TVar Mem.State) -- Try change Mem.MemState to TVar Mem.State
type AppState = TVar Mem.State

newtype App a = App { unApp :: ReaderT AppState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, KatipContext, Katip)


instance AuthRepo App where
  addAuth = Mem.addAuth
  setEmailAsVerified = Mem.setEmailAsVerified
  findUserByAuth = Mem.findUserByAuth
  findEmailFromUserId = Mem.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = Mem.notifyEmailVerification

instance SessionRepo App where
  newSession = Mem.newSession
  findUserIdBySessionId = Mem.findUserIdBySessionId




runState :: LogEnv -> AppState -> App a -> IO a
runState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp
  
 
withState :: (Int -> LogEnv -> AppState -> IO a) -> IO a
withState action = do
  let port = 3000

  withKatip $ \le -> do 
    memState <- newTVarIO Mem.initialState
    -- let appState = (pgState, redisState, mqState, memState) 
    let appState = memState
    action port le appState 



runRoutine :: IO ()
runRoutine = do
  withState $ \port le appState -> do
    let runner = runState le appState
    -- MQAuth.init mqState runner
    HTTP.main port runner


routine :: App ()
routine = do
  emailFileContent <- liftIO $ T.pack <$> Prelude.readFile "test-email.cfg"
  liftIO $ putStrLn emailFileContent
  let email = either undefined id $ mkEmail emailFileContent
  let passw = either undefined id $ mkPassword "123456Hello"
  let auth = Auth email passw
  _ <- register auth
  vCode <- App $ pollNotif email
  -- Just vCode <- App $ Mem.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  liftIO $ print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- Mem.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode
        