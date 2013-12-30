{-# LANGUAGE OverloadedStrings #-}
module Application where

import Blog.Auth
import Blog.Common
import Blog.Controllers.CommentsController
import Blog.Controllers.PostsController
import Network.Wai.Middleware.MethodOverridePost
import Web.Simple
import Web.Simple.Session
import Web.REST (restIndex, routeREST)

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newAppSettings

  runner $ methodOverridePost $
    controllerApp settings $ withSession $ do
      openIdController handleLogin
      routeName "login" loginPage
      routeName "logout" logout

      routeName "admin" $ do
        routeName "posts" $ do
          routePattern ":post_id/comments" $ commentsAdminController
          postsAdminController
        routeTop $ respond $ redirectTo "/admin/posts/"
      routeName "posts" $ do
        routePattern ":post_id/comments" $ commentsController
        routeREST $ postsController
      routeName "feed" atomFeed
      routeTop $ restIndex $ postsController
      serveStatic "static"

