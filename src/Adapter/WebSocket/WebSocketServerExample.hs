{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Adapter.WebSocket.WebSocketServerExample where

import ClassyPrelude
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as TIO



meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn :: IO Text
    TIO.putStrLn msg
    WS.sendTextData conn msg
    -- WS.sendTextData conn $ msg `T.append` ", meow"


-- Define the WebSocket application
app :: (WS.Connection -> IO ()) -> WS.ServerApp
app act = \pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  putStrLn "WebSocket connection established."
  
  act conn
  -- Perform your WebSocket-specific logic here
  -- For example, you can use 'sendTextData' and 'receiveData' to communicate with the client.
  
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
startWebSocketServer :: Int -> (WS.Connection -> IO ()) -> IO ()
startWebSocketServer port act = do
  WS.runServer "0.0.0.0" port (app act)

main :: IO ()
main = do
  putStrLn "Starting WebSocket server on port 1234..."
  startWebSocketServer 1234 meow



-- withPingPongUnlifted :: MonadUnliftIO m => PingPongOptions -> Connection -> (Connection -> m ()) -> m ()
-- withPingPongUnlifted options connection app = withRunInIO $ \run ->
--     withPingPong options connection (run . app)

