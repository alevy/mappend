{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Blog.Common
  ( AppSettings, newAppSettings, BlogSettings, AdminSettings
  , verifyCSRF, httpManager, remoteIp
  , baseDomain, withBlogDomain, currentBlog, dashboard
  , routeAny
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
import System.Environment (getEnv)

data AppSettings = AppSettings { appDB :: PostgreSQLConn
                               , appHttpManager :: Manager
                               , appBaseDomain :: S8.ByteString
                               , appSession :: Maybe Session }

data BlogSettings = BlogSettings { blogAppSettings :: AppSettings
                                  , blogBlog :: Blog }

data AdminSettings = AdminSettings { adminAppSettings :: AppSettings
                                   , adminBlog :: Blog }

newAppSettings :: IO AppSettings
newAppSettings = do
  envBaseDomain <- S8.pack <$> getEnv "BASE_DOMAIN"
  db <- createPostgreSQLConn
  mgr <- newManager defaultManagerSettings
  return $ AppSettings { appDB = db
                       , appSession = Nothing
                       , appBaseDomain = envBaseDomain
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

baseDomain :: Controller AppSettings S8.ByteString
baseDomain = appBaseDomain <$> controllerState

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

instance HasPostgreSQL AdminSettings where
  postgreSQLConn = appDB . adminAppSettings

instance HasSession AdminSettings where
  getSession = appSession . adminAppSettings
  setSession sess = do
    cs <- controllerState
    putState $ cs { adminAppSettings =
        (adminAppSettings cs) { appSession = Just sess }
      }

instance HasTemplates IO AdminSettings where
  defaultLayout = Just <$> getTemplate "layouts/admin.html"
  functionMap = return $ defaultFunctionMap <> helperFunctions
  layoutObject pageContent pageValue = do
    (Object obj) <- defaultLayoutObject pageContent pageValue
    blog <- currentBlog
    return $ Object $ H.insert "blog" (toJSON blog) obj

class HasBlog s where
  currentBlog :: Controller s Blog

instance HasBlog BlogSettings where
  currentBlog = blogBlog <$> controllerState

instance HasBlog AdminSettings where
  currentBlog = adminBlog <$> controllerState

verifyCSRF :: HasSession s => [(S.ByteString, S.ByteString)] -> Controller s ()
verifyCSRF params = do
  sessionCsrf <- sessionLookup "csrf_token"
  let formCsrf = lookup "csrf_token" params
  when (any isNothing [sessionCsrf, formCsrf] || sessionCsrf /= formCsrf) $
    respond badRequest

httpManager :: Controller BlogSettings Manager
httpManager = (appHttpManager . blogAppSettings) <$> controllerState

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

dashboard :: Controller AdminSettings () -> Controller AppSettings ()
dashboard act = do
  mbloggerId <- sessionLookup "blogger_id"
  case mbloggerId of
    Just blogId -> do
      mblog <- withConnection $ \conn -> liftIO $
        findRow conn $ read $ S8.unpack $ blogId
      case mblog of
        Just blog -> do
          st <- controllerState
          req <- request
          let newst = AdminSettings { adminAppSettings = st, adminBlog = blog }
          (eres, s) <- liftIO $ runController act newst req
          putState $ adminAppSettings s
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

routeAny :: [Controller s () -> Controller s ()] -> Controller s ()
         -> Controller s ()
routeAny routes ctrl = do
  forM_ routes $ \rt -> rt ctrl

