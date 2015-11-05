{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.CommentsController where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding
import Data.Time.LocalTime
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.Frank

import Blog.Common
import qualified Blog.Models ()
import qualified Blog.Models.Comment as C
import Blog.Models.Post

import Blog.Auth
import Blog.Models

commentsController :: Controller AppSettings ()
commentsController = do
  post "/" $ do
    pid <- readQueryParam' "post_id"
    (params, _) <- parseForm
    curTime <- liftIO $ getZonedTime
    comment <- C.testSpam $ C.commentFromForm params pid curTime

    withConnection $ \conn -> do
      mpost <- liftIO $ findRow conn pid
      when (isNothing mpost) $ respond notFound
      let myPost = fromJust mpost

      ec <- liftIO $ trySave conn comment
      case ec of
        Right _ -> respond $ redirectTo $
          encodeUtf8 $ "/posts/" <> (postSlug myPost)
        Left errs -> do
          comments <- liftIO $ allComments conn myPost
          render "posts/show.html" $
            object ["comment" .= comment, "errors" .= errs
                   , "post" .= myPost, "comments" .= comments]

commentsAdminController :: Controller AppSettings ()
commentsAdminController = requiresAdmin "/login" $ do
  get "/" $ withConnection $ \conn -> do
    pid <- readQueryParam' "post_id"
    (Just p) <- liftIO $ findRow conn pid
    comments <- liftIO $ allComments conn p
    csrf <- sessionLookup "csrf_token"
    renderLayout "layouts/admin.html"
      "admin/comments/index.html" $
        object [ "post" .= p, "comments" .= comments
               , "csrf_token" .= fmap decodeUtf8 csrf ]

  delete ":id" $ do
    cid <- readQueryParam' "id"
    (params, _) <- parseForm
    verifyCSRF params
    withConnection $ \conn -> liftIO $ do
      (Just comment) <- findRow conn cid :: IO (Maybe C.Comment)
      destroy conn comment
    redirectBack
