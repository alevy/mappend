{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Blog.Common
  ( AppSettings, newAppSettings, BlogSettings, verifyCSRF, httpManager, remoteIp
  , withBlogDomain, currentBlog
  , module Web.Simple.PostgreSQL
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Value(Object), toJSON)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Socket
import Web.Simple
import Web.Simple.PostgreSQL
import Web.Simple.Templates
import Web.Simple.Session
import Blog.Helpers
import Blog.Models.Blog (Blog, findByUsername)

data AppSettings = AppSettings { appDB :: PostgreSQLConn
                               , appHttpManager :: Manager
                               , appSession :: Maybe Session }

data BlogSettings = BlogSettings { blogAppSettings :: AppSettings
                                  , blogBlog :: Blog }

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

instance HasPostgreSQL BlogSettings where
  postgreSQLConn = appDB . blogAppSettings

instance HasSession BlogSettings where
  getSession = appSession . blogAppSettings
  setSession sess = do
    cs <- controllerState
    putState $ cs { blogAppSettings =
        (blogAppSettings cs) { appSession = Just sess }
      }

instance HasTemplates IO BlogSettings where
  defaultLayout = Just <$> getTemplate "layouts/blog.html"
  functionMap = return $ defaultFunctionMap <> helperFunctions
  layoutObject pageContent pageValue = do
    (Object obj) <- defaultLayoutObject pageContent pageValue
    blog <- currentBlog
    return $ Object $ H.insert "blog" (toJSON blog) obj

verifyCSRF :: HasSession s => [(S.ByteString, S.ByteString)] -> Controller s ()
verifyCSRF params = do
  sessionCsrf <- sessionLookup "csrf_token"
  let formCsrf = lookup "csrf_token" params
  when (any isNothing [sessionCsrf, formCsrf] || sessionCsrf /= formCsrf) $
    respond badRequest

httpManager :: Controller BlogSettings Manager
httpManager = (appHttpManager . blogAppSettings) <$> controllerState

currentBlog :: Controller BlogSettings Blog
currentBlog = blogBlog <$> controllerState

withBlogDomain :: Controller BlogSettings () -> Controller AppSettings ()
withBlogDomain act = do
  mhostname <- (return . fmap (S8.split '.'))=<< requestHeader "Host"
  case mhostname of
    (Just (username:_)) -> do
      mblog <- withConnection $ \conn -> liftIO $
        findByUsername conn $ decodeUtf8 username
      case mblog of
        Just blog -> do
          st <- controllerState
          req <- request
          let newst = BlogSettings { blogAppSettings = st, blogBlog = blog }
          (eres, s) <- liftIO $ runController act newst req
          putState $ blogAppSettings s
          case eres of
            Left resp -> respond resp
            Right a -> return a
        Nothing -> return ()
    _ -> return ()

remoteIp :: Controller s S.ByteString
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

