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
  , lobbyRoomIdToRoomId
  , newRoomData
  , roomIdToAcrhiveRoomId
  )
 where


import ClassyPrelude
import Domain.Auth (UserId)

newtype RoomId = RoomId Text
  deriving (Show, Eq, Ord)

newtype LobbyRoomId = LobbyRoomId Text
  deriving (Show, Eq, Ord)

newtype ArchiveRoomId = ArchiveRoomId Text
  deriving (Show, Eq, Ord)
  
newtype UserHost = UserHost {unUserHost :: UserId}
  deriving (Show, Eq, Ord)

newtype UserGuest = UserGuest {unUserGuest :: UserId}
  deriving (Show, Eq, Ord)

data JoinRoomError = JoinRoomErrorRoomDoesntExist
  deriving (Show, Eq, Ord)

data CloseRoomError = CloseRoomErrorRoomDoesntExist
  deriving (Show, Eq, Ord)

data RoomData = RoomData {userHost :: UserHost, userGuest :: UserGuest}
  deriving (Show, Eq, Ord)

newRoomData :: UserHost -> UserGuest -> RoomData
newRoomData hostId guestId = RoomData {userHost = hostId, userGuest = guestId}

lobbyRoomIdToRoomId :: LobbyRoomId -> RoomId
lobbyRoomIdToRoomId (LobbyRoomId roomId) = RoomId roomId

roomIdToAcrhiveRoomId :: RoomId -> ArchiveRoomId
roomIdToAcrhiveRoomId (RoomId roomId) = ArchiveRoomId roomId

class Monad m => RoomRepo m where
  createRoom :: UserHost -> m LobbyRoomId
  getOpenRooms :: m [LobbyRoomId]
  joinRoom :: UserGuest  -> LobbyRoomId -> m (Either JoinRoomError RoomId)
  closeRoom :: RoomId -> m (Either CloseRoomError ArchiveRoomId)