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
import Domain.Room (RoomId (..), RoomRepo (..), LobbyRoomId (..))
import Katip
import Control.Concurrent (threadDelay)


pingTime :: Int
pingTime = 1000


wsSendGuestJoinedGameRoom :: WS.Connection -> RoomId -> IO ()
wsSendGuestJoinedGameRoom conn (RoomId rId) =
  WS.sendTextData conn ("guest_joined_room::" <> rId)


wsAction :: (SessionRepo m, KatipContext m, RoomRepo m, MonadUnliftIO m) => (m () -> IO ()) -> Maybe ByteString -> WS.Connection -> m ()
wsAction runner mayCookies conn = do
  case mayCookies of
    Nothing -> do
      katipAddNamespace "wsAction" $ $(logTM) WarningS $ ls ("attempt to join room without active websocket connection" :: Text)
      liftIO $ WS.sendTextData conn ("websocket handshake error: session is not active" :: Text)
    Just cookies -> do
      case splitAt (length ("session_id=" :: String)) cookies of
        ("session_id=", sIdBS) -> do
          let sId = decodeUtf8 sIdBS :: Text
          addWSResult <- addWSConnection sId conn
          case addWSResult of
            Left SessionErrorSessionIsNotActive -> liftIO $ WS.sendTextData conn ("websocket handshake error: session is not active" :: Text)
            Right () -> 
              liftIO $ WS.withPingThread conn pingTime (pure ()) $ do
                WS.sendTextData conn ("ok" :: Text)
                catch
                  (runner $ wsThreadMessageListener conn sId)
                  (\(e :: SomeException) -> putStrLn ("WebSocket thread error: " <> tshow e) >> disconnect sId)
        (_,_) -> liftIO $ WS.sendTextData conn ("websocket handshake error: no session_id provided" :: Text)

  where
    disconnect sId = do
      putStrLn $ sId <> " disconnected"


wsThreadMessageListener :: (SessionRepo m, RoomRepo m, MonadUnliftIO m) => WS.Connection -> SessionId -> m ()
wsThreadMessageListener conn sId = forever $ do
    msg  <- liftIO $ WS.receiveData conn
    case msg of
      "enter_lobby" -> do
        liftIO $ WS.sendTextData conn ("Salam!" :: Text)
        putStrLn $ "RECIEVE ENTER LOBBY #(" <> sId <> "): " <> msg
        _ <- async $ forever $ do
            rooms <- getOpenRooms
            let roomsTxt = intercalate ";" $ map unLobbyRoomId rooms :: Text
            liftIO $ WS.sendTextData conn ("lobby_list::" <> roomsTxt)
            liftIO $ threadDelay 1_000_000  -- 1 second
        pure ()
      _ -> do
        liftIO $ WS.sendTextData conn ("Hi there!11" :: Text)
        putStrLn $ "RECIEVE #(" <> sId <> "): " <> msg


-- Define the WebSocket application
wsServer :: (Maybe ByteString -> WS.Connection -> IO ()) -> WS.ServerApp
wsServer action = \pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  
  let mayCookies = getWSCookies pendingConn
  action mayCookies conn
  
  -- Ensure the connection is properly closed when the action is complete
  finally
    (do
      putStrLn "WebSocket connection closed."
      WS.sendClose conn ("Closing connection" :: Text))
    (catchConnectionException conn)


getWSCookies :: WS.PendingConnection -> Maybe ByteString
getWSCookies pendingConn = do
  let requestHead = WS.pendingRequest pendingConn
  let headers = WS.requestHeaders requestHead
  let maybeCookies = lookup "Cookie" headers
  maybeCookies


-- Function to handle connection exceptions
catchConnectionException :: WS.Connection -> IO ()
catchConnectionException conn = do
  catch
    (void $ WS.receiveDataMessage conn) -- Attempt to receive data (use your specific logic here)
    (\(_ :: WS.ConnectionException) -> pure ()) -- Handle connection exceptions


-- Function to start the WebSocket server
initWebSocketServer :: MonadIO m => Int -> (Maybe ByteString -> WS.Connection -> IO ())  -> m ()
initWebSocketServer port action = do
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."
  liftIO $ WS.runServer "0.0.0.0" port (wsServer action)


runWebSocketServer :: (SessionRepo m, KatipContext m, RoomRepo m, MonadUnliftIO m) => (m () -> IO ()) -> IO ()
runWebSocketServer runner = initWebSocketServer 1234 (\bs conn -> runner $ wsAction runner bs conn)

