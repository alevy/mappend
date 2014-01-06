{-# LANGUAGE OverloadedStrings #-}
module Blog.Common
  ( AppSettings, newAppSettings, verifyCSRF
  , module Web.Simple.PostgreSQL
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as S
import Data.Maybe
import Data.Monoid
import Web.Simple
import Web.Simple.PostgreSQL
import Web.Simple.Templates
import Web.Simple.Session
import Blog.Helpers

data AppSettings = AppSettings { appDB :: PostgreSQLConn
                               , appSession :: Maybe Session }

newAppSettings :: IO AppSettings
newAppSettings = do
  db <- createPostgreSQLConn
  return $ AppSettings { appDB = db , appSession = Nothing }

instance HasPostgreSQL AppSettings where
  postgreSQLConn = appDB

instance HasSession AppSettings where
  getSession = appSession
  setSession sess = do
    cs <- controllerState
    putState $ cs { appSession = Just sess }

instance HasTemplates AppSettings where
  defaultLayout = Just <$> getTemplate "layouts/main.html"
  functionMap = return $ defaultFunctionMap <> helperFunctions

verifyCSRF :: [(S.ByteString, S.ByteString)] -> Controller AppSettings ()
verifyCSRF params = do
  sessionCsrf <- sessionLookup "csrf_token"
  let formCsrf = lookup "csrf_token" params
  when (any isNothing [sessionCsrf, formCsrf] || sessionCsrf /= formCsrf) $
    respond badRequest
