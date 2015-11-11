{-# LANGUAGE OverloadedStrings #-}
module Blog.Auth where

import Prelude hiding (div)

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Hex
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe
import Database.PostgreSQL.Simple
import Network.HTTP.Conduit (withManager)
import System.Entropy
import Web.Frank
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.Authenticate.OpenId

import Blog.Common

openIdController :: (T.Text -> Controller a ()) -> Controller a ()
openIdController loginHandler = do
  get "auth/finalize" $ do
    prms <- (map (\(k,(Just v)) -> (decodeUtf8 k, decodeUtf8 v)))
              <$> queryString <$> request
    oidr <- liftIO $ withManager $ authenticateClaimed prms
    case identifier <$> oirClaimed oidr of
      Just openid -> do
        loginHandler openid
      _ -> respond forbidden
  get "auth/login" $ do
    claimedId <- queryParam' "openid_identifier"
    (Just host) <- requestHeader "Host"
    secure <- isSecure <$> request
    let completePage = decodeUtf8 $
          if secure then
            S8.concat ["https://", host, "/auth/finalize"]
            else S8.concat ["http://", host, "/auth/finalize"]
    fu <- liftIO $ withManager $ getForwardUrl claimedId
                    completePage Nothing []
    respond $ redirectTo $ encodeUtf8 fu

handleLogin :: T.Text -> Controller AppSettings ()
handleLogin openid = do
  res <- withConnection $ \conn -> liftIO $
    query_ conn "select openid from admins"
  when (length res == 0 || head res /= (Only openid)) $
    respond forbidden
  ret <- fromMaybe "/" `fmap` sessionLookup "return_to"
  sessionDelete "return_to"
  sessionInsert "user" $ encodeUtf8 openid
  csrfToken <- liftIO $ hex <$> getEntropy 32
  sessionInsert "csrf_token" $ csrfToken
  respond $ redirectTo ret

logout :: Controller AppSettings ()
logout = do
  sessionDelete "user"
  respond $ redirectTo "/"

requiresAdmin :: S8.ByteString
              -> Controller AppSettings b -> Controller AppSettings b
requiresAdmin loginUrl cnt = do
  muser <- sessionLookup "user"
  if isJust muser then
    cnt
    else do
      req <- request
      sessionInsert "return_to" $ rawPathInfo req
      respond $ redirectTo loginUrl

loginPage :: Controller AppSettings ()
loginPage = renderLayout "layouts/login.html" "login.html" Null

