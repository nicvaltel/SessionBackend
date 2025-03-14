{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Common where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
-- import qualified Text.Digestive.Form as DF
-- import qualified Text.Digestive.Aeson as DF
import Data.Aeson
import Network.HTTP.Types.Status
import Adapter.HTTP.Common
import Data.Aeson.Key (fromText)


-- -- * Forms

-- parseAndValidateJSON :: (MonadUnliftIO m, ToJSON v) => DF.Form v m a -> ActionT m a
-- parseAndValidateJSON form = do
--   val <- jsonData `catch` (\(_:: SomeException) -> pure Null)
--   validataionResult <- lift $ DF.digestJSON form val
--   case validataionResult of
--     (v, Nothing) -> do
--       status status400
--       json $ DF.jsonErrors v
--       finish
--     (_, Just result) -> pure result

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

-- * Error responce

errorResponce :: (ToJSON a) => a -> Value
errorResponce val = object [ "error" .= val]


jsonResponce :: [(Text, Text)] -> Value
jsonResponce keyValPairs = object [ fromText k .= v | (k, v) <- keyValPairs]