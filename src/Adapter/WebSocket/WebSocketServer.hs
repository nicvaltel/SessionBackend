{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
-- websocat -v ws://127.0.0.1:1234
module Adapter.WebSocket.WebSocketServer 
  ( initWebSocketServer, 
    runWebSocketServer, 
    wsSendGuestJoinedGameRoom
  ) where

import ClassyPrelude
import qualified Network.WebSockets as WS
import Domain.Auth
import qualified Data.Text as T
import Domain.Room (RoomId (..))
import Katip

pingTime :: Int
pingTime = 1000


wsSendGuestJoinedGameRoom :: WS.Connection -> RoomId -> IO ()
wsSendGuestJoinedGameRoom conn (RoomId rId) =
  WS.sendTextData conn ("guest_joined_room::" <> rId)



wsAction :: (SessionRepo m, KatipContext m) => WS.Connection -> m ()
wsAction conn = do
  msg :: Text <- liftIO $ WS.receiveData conn
  case T.breakOn "::" msg of
    ("session_id",sId') -> do
      let sId = T.init $ T.drop (T.length "::") sId' -- TODO delete "\n" at the end of ws messages
      print sId
      katipAddNamespace "wsAction" $ $(logTM) WarningS $ ls $ "attempt to join room without active websocket connection userId=" <> tshow 111
      -- logAction WarningS $ ls $ "attempt to join room without active websocket connection userId=" <> tshow guestId <> ", sessionId=" <> sId
      addWSResult <- addWSConnection sId conn
      case addWSResult of
        Left SessionErrorSessionIsNotActive -> liftIO $ WS.sendTextData conn ("websocket handshake error: session is not active" :: Text)
        Right () -> liftIO $ WS.withPingThread conn pingTime (pure ()) $ do
            liftIO $ WS.sendTextData conn ("ok" :: Text)
            catch
              (wsThreadMessageListener conn sId)
              (\(e :: SomeException) -> putStrLn ("WebSocket thread error: " <> tshow e) >> disconnect sId)

    _ -> liftIO $ WS.sendTextData conn ("websocket handshake error: no session_id provided" :: Text)

  where

    disconnect sId = do
      putStrLn $ sId <> " disconnected"


wsThreadMessageListener :: WS.Connection -> SessionId -> IO ()
wsThreadMessageListener conn sId = forever $ do
    msg  <- WS.receiveData conn
    WS.sendTextData conn ("Hi there!11" :: Text)
    putStrLn $ "RECIEVE #(" <> sId <> "): " <> msg



-- Define the WebSocket application
wsServer :: (WS.Connection -> IO ()) -> WS.ServerApp
wsServer action = \pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  putStrLn "WebSocket connection established."
  
  action conn
  -- Ensure the connection is properly closed when the action is complete
  finally
    (do
      putStrLn "WebSocket connection closed."
      WS.sendClose conn ("Closing connection" :: Text))
    (catchConnectionException conn)

-- Function to handle connection exceptions
catchConnectionException :: WS.Connection -> IO ()
catchConnectionException conn = do
  catch
    (void $ WS.receiveDataMessage conn) -- Attempt to receive data (use your specific logic here)
    (\(_ :: WS.ConnectionException) -> pure ()) -- Handle connection exceptions

-- Function to start the WebSocket server
initWebSocketServer :: MonadIO m => Int -> (WS.Connection -> IO ())  -> m ()
initWebSocketServer port action = do
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."
  liftIO $ WS.runServer "0.0.0.0" port (wsServer action)

runWebSocketServer :: (SessionRepo m, KatipContext m) => (m () -> IO ()) -> IO ()
runWebSocketServer runner = initWebSocketServer 1234 (runner . wsAction)


-- runWebSocketServer :: (MonadUnliftIO m, SessionRepo m) => (m () -> IO ()) -> IO ()
-- runWebSocketServer runner = runner $ do
--   putStrLn "Starting WebSocket server on port 1234..."
--   withRunInIO $ \run ->
--     initWebSocketServer 1234 (run . wsAction)

-- withPingPongUnlifted :: MonadUnliftIO m => PingPongOptions -> Connection -> (Connection -> m ()) -> m ()
-- withPingPongUnlifted options connection app = withRunInIO $ \run ->
--     withPingPong options connection (run . app)
