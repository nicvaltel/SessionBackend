{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Web.Main (main) where


import ClassyPrelude
import Domain.Auth
-- import Web.Scotty
import qualified Adapter.HTTP.Common as WebCommon
import Web.Cookie (SetCookie(..))
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Static (staticPolicy', CacheContainer, addBase, initCaching, CachingStrategy (..), staticPolicy)
import Katip (KatipContext, logTM, Severity (..), ls)
-- import qualified Adapter.HTTP.Web.Auth as WebAuth


main :: 
  (MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) =>
  (m Response -> IO Response) -> IO Application
main runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT defaultOptions runner $ routes cacheContainer
  -- scottyAppT defaultOptions runner routes

routes :: 
  ( MonadUnliftIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m) =>
  CacheContainer -> ScottyT m ()
routes cachingStrategy= do

  middleware $ gzip $ def {gzipFiles = GzipCompress}
  -- middleware $ staticPolicy' cachingStrategy (addBase "src/Adapter/HTTP/Web")
  -- middleware $ staticPolicy' cachingStrategy (addBase "static") 
  middleware $ staticPolicy  (addBase "static") 

  -- WebAuth.routes
  get "/" $ file "static/html/index.html" -- Serve the main page
  get "/index" $ file "static/html/index.html"  
  get "/index.html" $ file "static/html/index.html"

  get "/register" $ file "static/html/register.html"  

  get "/verify" $ file "static/html/verify.html"

  get "/login" $ file "static/html/login.html"

  get "/create-room" $ file "static/html/create-room.html"

  get "/lobby" $ file "static/html/lobby.html"

  get "/game/:game_id" $ file "static/html/game.html"

  notFound $ do
    status status404
    text "Not found"

  defaultHandler $ Handler $ \(e :: SomeException) -> do
    lift $ $(logTM) ErrorS $ "Unhandeled error: " <> ls (show e)
    status status500
    text "InternalServerError"

-- -- Serve static files with proper MIME type
-- serveStatic :: FilePath -> ActionM ()
-- serveStatic path = do
--     setHeader "Content-Type" $ defaultMimeLookup path
--     file path


-- test :: IO ()
-- test = scotty 3000 $ do
--   get "/aaa/:word" $ do
--     beam <- pathParam "word"
--     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  
--   get "/:word" $ do
--     beam <- pathParam "word"
--     text $ mconcat ["<h1>Panki Hoy! , ", beam, " me up!</h1>"]

--   get "/setter/:word" $ do
--     WebCommon.setSessionIdInCookie "SALAM!!!"
--     beam <- pathParam "word"
--     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

--   get "/getter/:word" $ do
--     cookie <- WebCommon.getCookie "sId"
--     case cookie of
--       Nothing -> html $ mconcat ["<h1>Cookie returns nothing!</h1>"]
--       Just sessionId -> do
--         beam <- pathParam "word"
--         html $ mconcat ["<h1>Cookie = , ", fromStrict sessionId, " " , beam, " me up!</h1>"]
    