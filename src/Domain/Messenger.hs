{-# LANGUAGE ScopedTypeVariables #-}
module Domain.Messenger where

import ClassyPrelude
import Domain.Auth
import Domain.Room
import Control.Concurrent (threadDelay)


type RecieveMessage = IO Text
type SendMessage = Text -> IO ()

data InputMessage = 
    InMsgEnterLobby
  | InMsgOther
  deriving (Show, Eq, Ord)

parseInMessage :: Text -> InputMessage
parseInMessage "enter_lobby" = InMsgEnterLobby
parseInMessage _ = InMsgOther

data OutputMessage =
    OutMsgLobbyList [LobbyRoomId]
  | OutMsgOk
  deriving (Eq, Ord)


tshowOutMessage :: OutputMessage -> Text
tshowOutMessage (OutMsgLobbyList rooms) = "lobby_list::" <> (intercalate ";" $ map unLobbyRoomId rooms :: Text)
tshowOutMessage OutMsgOk = "ok"


messageListener :: forall m. (SessionRepo m, RoomRepo m, MonadUnliftIO m) => RecieveMessage -> SendMessage -> SessionId -> m ()
messageListener recieveMessage sendMessage sId = do
  let sendMsg = liftIO . sendMessage . tshowOutMessage :: OutputMessage -> m ()
  msgTxt <- liftIO recieveMessage
  let msg = parseInMessage msgTxt
  case msg of
    InMsgEnterLobby -> do
      putStrLn $ "RECIEVE ENTER LOBBY #(" <> sId <> "): " <> tshow msg
      _ <- async $ forever $ do
          rooms <- getOpenRooms
          sendMsg $ OutMsgLobbyList rooms
          liftIO $ threadDelay 1_000_000  -- 1 second
      pure ()
    InMsgOther -> do
      putStrLn $ "RECIEVE #(" <> sId <> "): " <> tshow msg