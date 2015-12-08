{-# LANGUAGE OverloadedStrings #-}
module Blog.Auth where

import Prelude hiding (div)

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Hex
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe
import System.Entropy
import Web.Frank
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates

import Blog.Common
import Blog.Models.Blog (blogId, blogLogin)

logout :: Controller AdminSettings ()
logout = do
  sessionDelete "blogger_id"
  respond $ redirectTo "/"

requiresAdmin :: S8.ByteString
              -> Controller AdminSettings b -> Controller AdminSettings b
requiresAdmin loginUrl cnt = do
  mbid <- sessionLookup "blogger_id"
  curBlog <- currentBlog
  case mbid of
    Just bid | (DBKey $ read . S8.unpack $ bid) == blogId curBlog -> cnt
    _ -> do
      req <- request
      sessionInsert "return_to" $ rawPathInfo req
      respond $ redirectTo loginUrl

loginPage :: Controller AppSettings ()
loginPage = do
  get "/" $ render "main/login.html" ()

  post "/" $ do
    params <- fst <$> parseForm
    let username = decodeUtf8 <$> fromMaybe "" $ lookup "username" params
        password = decodeUtf8 <$> fromMaybe "" $ lookup "password" params
    muser <- withConnection $ \conn -> liftIO $ blogLogin conn username password
    case muser of
      Nothing -> do
        render "main/login.html" $ object
          [ "error" .= ("Incorrect username/password" :: T.Text)]
      Just  blog -> do
        sessionInsert "blogger_id" $ S8.pack $ show $ blogId blog
        ret <- fromMaybe "/dashboard" `fmap` sessionLookup "return_to"
        sessionDelete "return_to"
        csrfToken <- liftIO $ hex <$> getEntropy 32
        sessionInsert "csrf_token" $ csrfToken
        respond $ redirectTo ret

