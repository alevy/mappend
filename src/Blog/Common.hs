{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Blog.Common
  ( AppSettings, newAppSettings, verifyCSRF, httpManager, remoteIp
  , module Web.Simple.PostgreSQL
  ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Socket
import Web.Simple
import Web.Simple.PostgreSQL
import Web.Simple.Templates
import Web.Simple.Session
import Blog.Helpers

data AppSettings = AppSettings { appDB :: PostgreSQLConn
                               , appHttpManager :: Manager
                               , appSession :: Maybe Session }

newAppSettings :: IO AppSettings
newAppSettings = do
  db <- createPostgreSQLConn
  mgr <- newManager defaultManagerSettings
  return $ AppSettings { appDB = db
                       , appSession = Nothing
                       , appHttpManager = mgr }

instance HasPostgreSQL AppSettings where
  postgreSQLConn = appDB

instance HasSession AppSettings where
  getSession = appSession
  setSession sess = do
    cs <- controllerState
    putState $ cs { appSession = Just sess }

instance HasTemplates IO AppSettings where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  functionMap = return $ defaultFunctionMap <> helperFunctions

verifyCSRF :: [(S.ByteString, S.ByteString)] -> Controller AppSettings ()
verifyCSRF params = do
  sessionCsrf <- sessionLookup "csrf_token"
  let formCsrf = lookup "csrf_token" params
  when (any isNothing [sessionCsrf, formCsrf] || sessionCsrf /= formCsrf) $
    respond badRequest

httpManager :: Controller AppSettings Manager
httpManager = appHttpManager <$> controllerState

remoteIp :: Controller AppSettings S.ByteString
remoteIp = do
  req <- request
  let hdrs = requestHeaders req
      mrip = listToMaybe . catMaybes $
              [lookup "x-real-ip" hdrs
              , S8.takeWhile (/= ',') <$> lookup "x-forwarded-for" hdrs]
  nonProxyIp <- liftIO $
    getNameInfo [NI_NUMERICHOST] True False $ remoteHost req
  return $ case mrip of
    Just ip -> ip
    Nothing -> S8.pack <$> fromMaybe "no-client-address" $ fst nonProxyIp

