{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Common where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import Data.Aeson
import Network.HTTP.Types.Status
import Adapter.HTTP.Common
import Data.Aeson.Key (fromText)

-- * Sessions
reqCurrentUserId :: (SessionRepo m, MonadIO m) => ActionT m UserId
reqCurrentUserId = do
  mayUser <- getCurrentUserId
  case mayUser of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId ->
      pure userId

-- * JSON responce
jsonResponce :: [(Text, Text)] -> Value
jsonResponce keyValPairs = object [ fromText k .= v | (k, v) <- keyValPairs]

errorResponce :: (ToJSON a) => a -> Value
errorResponce val = object [ "error" .= val]
