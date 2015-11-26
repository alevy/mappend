{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PreferencesController where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.Frank (get, post)

import Blog.Auth (requiresAdmin)
import Blog.Common
import Blog.Models.Blog

preferencesController :: Controller BlogSettings ()
preferencesController = requiresAdmin "/login" $ do
  blog <- currentBlog
  csrf <- sessionLookup "csrf_token"

  get "/" $ renderLayout "layouts/admin.html" "admin/preferences.html" $
    object [ "blog" .= blog, "csrf_token" .= fmap decodeUtf8 csrf ]

  post "/" $ do
    params <- fst <$> parseForm
    let title = decodeUtf8 $ fromMaybe "" $ lookup "title" params
    let newblog = blog { blogTitle = title }
    eres <- withConnection $ \conn ->
      liftIO $ trySave conn newblog
    case eres of
      Left errs -> renderLayout "layouts/admin.html" "admin/preferences.html" $
        object [ "blog" .= newblog, "errors" .= errs ]
      Right _ -> redirectBack

