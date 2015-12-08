{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.MainController where

import Web.Frank
import Web.Simple

import Blog.Auth
import Blog.Controllers.CommentsController (commentsAdminController)
import Blog.Controllers.PostsController (postsAdminController)
import Blog.Controllers.PreferencesController (preferencesController)
import Blog.Common

mainController :: Controller AppSettings ()
mainController = do
  routeAny [routeTop, routeName "/register" . routeTop] $ register
  routeName "login" $ login

  get "/logout" logout

  routeName "dashboard" $ do
    dashboard $ requiresAdmin "/login" $ do
      get "/" $ respond $ redirectTo "/dashboard/posts/new"
      routeName "posts" $ do
        postsAdminController
        routePattern ":post_id/comments" $ commentsAdminController
      routeName "preferences" $ preferencesController

    redirectLogin "/login"

  serveStatic "static"
  respond $ notFound

