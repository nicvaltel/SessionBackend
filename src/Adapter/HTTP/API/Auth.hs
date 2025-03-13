module Adapter.HTTP.API.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Adapter.HTTP.API.Common
import Katip


routes :: (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) => ScottyT m ()
routes = do
  -- register
  post "/api/auth/register" $ do
    pure ()

  -- get user
  get "/api/users" $ do
    pure ()