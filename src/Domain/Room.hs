module Domain.Room 
  ( RoomId (..)
  , LobbyRoomId(..)
  , ArchiveRoomId(..)
  , UserHost(..)
  , UserGuest(..)
  , RoomData (..)
  , RoomRepo(..)
  , JoinRoomError(..)
  , CloseRoomError(..)
  , GameParams(..)
  , lobbyRoomIdToRoomId
  , newRoomData
  , roomIdToAcrhiveRoomId
  )
 where


import ClassyPrelude
import Domain.Auth (UserId)
import qualified Network.WebSockets as WS

newtype RoomId = RoomId Text
  deriving (Show, Eq, Ord)

newtype LobbyRoomId = LobbyRoomId {unLobbyRoomId :: Text}
  deriving (Show, Eq, Ord)

newtype ArchiveRoomId = ArchiveRoomId Text
  deriving (Show, Eq, Ord)
  
data UserHost = UserHost {hostId :: UserId, hostConn :: WS.Connection}

data UserGuest = UserGuest {guestId :: UserId, guestConn :: WS.Connection}

data JoinRoomError = JoinRoomErrorRoomDoesntExist
  deriving (Show, Eq, Ord)

data CloseRoomError = CloseRoomErrorRoomDoesntExist
  deriving (Show, Eq, Ord)

data RoomData = RoomData {userHost :: UserHost, userGuest :: UserGuest}


newRoomData :: UserHost -> UserGuest -> RoomData
newRoomData hostId guestId = RoomData {userHost = hostId, userGuest = guestId}

lobbyRoomIdToRoomId :: LobbyRoomId -> RoomId
lobbyRoomIdToRoomId (LobbyRoomId roomId) = RoomId roomId

roomIdToAcrhiveRoomId :: RoomId -> ArchiveRoomId
roomIdToAcrhiveRoomId (RoomId roomId) = ArchiveRoomId roomId

class Monad m => RoomRepo m where
  createRoom :: UserHost -> m LobbyRoomId
  getOpenRooms :: m [LobbyRoomId]
  joinRoom :: UserGuest  -> LobbyRoomId -> m (Either JoinRoomError (RoomId, UserHost))
  closeRoom :: RoomId -> m (Either CloseRoomError ArchiveRoomId)
  checkLobbyRoomIsGameStarted :: LobbyRoomId -> m (Maybe RoomId)


data GameParams = GameParamsCasual | GameParamsRated
  deriving (Show, Eq, Ord)