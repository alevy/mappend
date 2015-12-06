{-# LANGUAGE OverloadedStrings #-}
module Application where

import Data.Monoid ((<>))
import Blog.Common
import Blog.Controllers.CommentsController
import Blog.Controllers.MainController
import Blog.Controllers.PostsController
import Network.Wai.Middleware.MethodOverridePost
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.REST (restIndex, routeREST)

app :: (Application -> IO ()) -> IO ()
app runner = do
  settings <- newAppSettings

  runner $ methodOverridePost $
    controllerApp settings $ withSession $ do
      mainDomain <- baseDomain

      routeAny [routeHost mainDomain, routeHost $ "www." <> mainDomain] $
        mainController

      withBlogDomain $ do
        routeName "about" $ render "about.html" ()

        routeName "posts" $ do
          routePattern ":post_id/comments" $ commentsController
          routeREST $ postsController
        routeName "feed" atomFeed
        routeTop $ restIndex $ postsController
        serveStatic "static"
        respond notFound

      secure <- isSecure <$> request
      respond $ redirectTo $
        if secure then
          "https://" <> mainDomain
          else "http://" <> mainDomain

