{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
-- websocat -v ws://127.0.0.1:1234
module Adapter.WebSocket.WebSocketServer (initWebSocketServer, runWebSocketServer) where

import ClassyPrelude
import qualified Network.WebSockets as WS
import Domain.Auth


wsAction :: (MonadIO m, SessionRepo m) => WS.Connection -> m ()
wsAction conn = do
  let sId = "666777888" :: SessionId
  addWSConnection sId conn
  pure ()





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

runWebSocketServer :: (MonadUnliftIO m, SessionRepo m) => (m () -> IO ()) -> IO ()
runWebSocketServer runner = initWebSocketServer 1234 (runner . wsAction)


-- runWebSocketServer :: (MonadUnliftIO m, SessionRepo m) => (m () -> IO ()) -> IO ()
-- runWebSocketServer runner = runner $ do
--   putStrLn "Starting WebSocket server on port 1234..."
--   withRunInIO $ \run ->
--     initWebSocketServer 1234 (run . wsAction)

-- withPingPongUnlifted :: MonadUnliftIO m => PingPongOptions -> Connection -> (Connection -> m ()) -> m ()
-- withPingPongUnlifted options connection app = withRunInIO $ \run ->
--     withPingPong options connection (run . app)
