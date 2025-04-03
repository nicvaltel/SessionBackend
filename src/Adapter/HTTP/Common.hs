{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Common where

import ClassyPrelude
import Web.Scotty.Trans

import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Domain.Auth
import Data.Time.Lens
import Katip (KatipContext, katipAddNamespace, Namespace (Namespace))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

type CookieName = ByteString
type CookieValue = ByteString
type CookieSecure = Bool


liftKatipContext :: (KatipContext m) => m a -> ActionT m a
liftKatipContext = lift . katipAddNamespace (Namespace mempty)

setCookie :: MonadIO m => SetCookie -> ActionT m ()
setCookie = setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

setCookieDefault :: MonadIO m => CookieName -> CookieValue -> CookieSecure -> ActionT m ()
setCookieDefault cname cval secure = do
  curTime <- liftIO getCurrentTime
  setCookie $ def{
      setCookieName = cname
    , setCookiePath = Just "/"
    , setCookieValue = cval
    , setCookieExpires = Just $ modL month (+ 1) curTime
    , setCookieHttpOnly = True
    , setCookieSecure = secure
    , setCookieSameSite = Just sameSiteLax
    }

deleteCookieDefault :: MonadIO m => CookieName -> CookieSecure -> ActionT m ()
deleteCookieDefault cname secure =
  setCookie $ def{
      setCookieName = cname
    , setCookiePath = Just "/"
    , setCookieValue = ""
    , setCookieExpires = Just $ posixSecondsToUTCTime 0
    , setCookieHttpOnly = True
    , setCookieSecure = secure
    , setCookieSameSite = Just sameSiteLax
    }

getCookie :: Monad m => Text -> ActionT m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  pure $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    pure $ decodeUtf8 val


setSessionIdInCookie :: MonadIO m => SessionId -> ActionT m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $ def{
      setCookieName = "sId"
    , setCookiePath = Just "/"
    , setCookieValue = encodeUtf8 sId
    , setCookieExpires = Just $ modL month (+ 1) curTime
    , setCookieHttpOnly = True
    , setCookieSecure = False
    , setCookieSameSite = Just sameSiteLax
    }

getCurrentUserId :: SessionRepo m => ActionT m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> pure Nothing
    Just sId -> lift $ resolveSessionId sId
