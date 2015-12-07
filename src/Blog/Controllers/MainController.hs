{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.MainController where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Web.Frank
import Web.Simple
import Web.Simple.Templates

import Blog.Auth
import Blog.Controllers.CommentsController (commentsAdminController)
import Blog.Controllers.PostsController (postsAdminController)
import Blog.Controllers.PreferencesController (preferencesController)
import Blog.Common

mainController :: Controller AppSettings ()
mainController = do
  routeAny [get "/", get "/invite"] $ do
    inviteCode <- queryParam "invite_code"
    render "main/index.html" $
      object ["invite_code" .= (inviteCode :: Maybe Text) ]

  routeName "login" $ loginPage

  dashboard $ get "/logout" logout
  routeName "dashboard" $ dashboard $ requiresAdmin "/login" $ do
    get "/" $ respond $ redirectTo "/dashboard/posts/new"
    routeName "posts" $ do
      postsAdminController
      routePattern ":post_id/comments" $ commentsAdminController
    routeName "preferences" $ preferencesController

  serveStatic "static"
  respond $ notFound

