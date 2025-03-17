{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Main (main) where


import ClassyPrelude
import Domain.Auth
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as Auth
import Adapter.HTTP.API.Common
import Network.Wai
import Network.Wai.Middleware.Gzip
import Katip
import Domain.Room (RoomRepo)



main :: 
  (MonadUnliftIO m, KatipContext m, AuthRepo m, SessionRepo m, RoomRepo m) =>
  (m Response -> IO Response) -> IO Application
main runner = scottyAppT defaultOptions runner routes

routes :: 
  ( MonadUnliftIO m, KatipContext m, AuthRepo m , SessionRepo m, RoomRepo m) =>
  ScottyT m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}

  Auth.routes

  notFound $ do
    status status404
    json $ errorResponce ("NotFound" :: Text)

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    json ("InternalServerError" :: Text)