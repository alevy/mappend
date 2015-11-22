{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PostsController where

import Prelude hiding (show)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.List (partition)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.LocalTime (getZonedTime)
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.Frank (post)
import Web.REST

import Blog.Auth
import Blog.Common
import Blog.Helpers (markdown)
import Blog.Models
import Blog.Models.Post

atomFeed :: Controller BlogSettings ()
atomFeed = withConnection $ \conn -> do
  blog <- currentBlog
  now <- liftIO getZonedTime
  posts <- liftIO $ dbSelect conn $ addWhere_ "posted_at is not null"
                                  $ setLimit 10
                                  $ setOrderBy "posted_at desc"
                                  $ getPosts blog
  renderPlain "feed.atom" $
    object ["posts" .= (posts :: [Post]), "now" .= now]

postsController :: REST IO BlogSettings
postsController = rest $ do

  index $ withConnection $ \conn -> do
    mpage <- readQueryParam "offset"
    let page = maybe 0 id mpage
    posts <- liftIO $ dbSelect conn $ addWhere_ "posted_at is not null"
                                    $ setLimit 10
                                    $ setOffset (page * 10)
                                    $ setOrderBy "posted_at desc"
                                    $ modelDBSelect
    render "posts/index.html" (posts :: [Post])

  show $ do
    withConnection $ \conn -> do
      slug <- queryParam' "id"
      mpost <- liftIO $ listToMaybe <$>
        (dbSelect conn $
          addWhere "slug = ?" [slug :: S8.ByteString] $
          modelDBSelect)
      case mpost of
        Just p -> do
          comments <- liftIO $ allComments conn p
          render "posts/show.html" $
            object ["post" .= p, "comments" .= comments]
        Nothing -> respond notFound

postsAdminController :: Controller BlogSettings ()
postsAdminController = requiresAdmin "/login" $ do
  post "/preview" $ do
    (params, _) <- parseForm
    let mbody = decodeUtf8 <$> lookup "body" params
    case mbody of
      Nothing -> respond badRequest
      Just pBody -> respond $
                    okJson $ encode $ object ["body" .= markdown pBody]

  post ":id/unpublish" $ withConnection $ \conn -> do
    pid <- readQueryParam' "id"
    (params, _) <- parseForm
    verifyCSRF params
    mpost <- liftIO $ findRow conn pid
    case mpost of
      Just post0 -> do
        epost <- liftIO $ trySave conn $ post0 { postPostedAt = Nothing }
        case epost of
          Left errs -> do
            csrf <- sessionLookup "csrf_token"
            renderLayout "layouts/admin.html" "admin/posts/edit.html" $
              object [ "errors" .= errs, "post" .= post0
                     , "csrf_token" .= fmap decodeUtf8 csrf ]
          Right _ -> redirectBack
      Nothing -> redirectBack

  routeREST $ rest $ do
    index $ withConnection $ \conn -> do
      posts <- liftIO $ dbSelect conn $
        setOrderBy "posted_at desc" $ modelDBSelect
      let (published, drafts) = partition (isJust . postPostedAt) posts
      csrf <- sessionLookup "csrf_token"
      renderLayout "layouts/admin.html"
        "admin/posts/index.html" $
        object [ "published" .= published, "drafts" .= drafts
               , "csrf_token" .= fmap decodeUtf8 csrf]

    edit $ withConnection $ \conn -> do
      pid <- readQueryParam' "id"
      (Just p) <- liftIO $
        findRow conn pid :: Controller BlogSettings (Maybe Post)
      csrf <- sessionLookup "csrf_token"
      renderLayout "layouts/admin.html"
        "admin/posts/edit.html" $
          object ["post" .= p, "csrf_token" .= fmap decodeUtf8 csrf]

    update $ withConnection $ \conn -> do
      pid <- readQueryParam' "id"
      (Just p) <- liftIO $ findRow conn pid
      (params, _) <- parseForm
      verifyCSRF params
      curTime <- liftIO $ getZonedTime
      let mpost = do
            pTitle <- (decodeUtf8 <$> lookup "title" params) <|>
                        (pure $ postTitle p)
            pBody <- (decodeUtf8 <$> lookup "body" params) <|>
                      (pure $ postBody p)
            let postedAt = (lookup "publish" params >> pure curTime) <|>
                              postPostedAt p
            return $ p { postTitle = pTitle
                          , postBody = pBody
                          , postBodyHtml = markdown pBody
                          , postPostedAt = postedAt }
      case mpost of
        Just post0 -> do
          epost <- liftIO $ trySave conn post0
          case epost of
            Left errs -> do
              csrf <- sessionLookup "csrf_token"
              renderLayout "layouts/admin.html" "admin/posts/edit.html" $
                object [ "errors" .= errs, "post" .= post0
                       , "csrf_token" .= fmap decodeUtf8 csrf ]
            Right _ -> respond $ redirectTo "/admin/posts/"
        Nothing -> redirectBack

    new $ do
      csrf <- sessionLookup "csrf_token"
      renderLayout "layouts/admin.html"
        "admin/posts/new.html" $ object [ "csrf_token" .= fmap decodeUtf8 csrf ]

    create $ withConnection $ \conn -> do
      blog <- currentBlog
      (params, _) <- parseForm
      verifyCSRF params
      curTime <- liftIO $ getZonedTime
      let mpost = do
            pTitle <- decodeUtf8 <$> lookup "title" params
            pSummary <- decodeUtf8 <$> lookup "summary" params
            pBody <- decodeUtf8 <$> lookup "body" params
            pSlug <- (((not . T.null) `mfilter`
                        (decodeUtf8 <$> lookup "slug" params))
                      <|> (Just $ slugFromTitle pTitle))
            let postedAt = lookup "publish" params >> pure curTime
            return $ Post { postId = NullKey
                          , postBlogId = mkDBRef blog
                          , postTitle = pTitle
                          , postSummary = pSummary
                          , postSlug = pSlug
                          , postBody = pBody
                          , postBodyHtml = markdown pBody
                          , postPostedAt = postedAt }
      case mpost of
        Just post0 -> do
          epost <- liftIO $ trySave conn post0
          case epost of
            Left errs -> do
              csrf <- sessionLookup "csrf_token"
              renderLayout "layouts/admin.html" "admin/posts/new.html" $
                object [ "errors" .= errs, "post" .= post0
                       , "csrf_token" .= fmap decodeUtf8 csrf ]
            Right _ -> respond $ redirectTo "/admin/posts/"
        Nothing -> redirectBack

    delete $ withConnection $ \conn -> do
      pid <- readQueryParam' "id"
      (params, _) <- parseForm
      verifyCSRF params
      (Just p) <- liftIO $ findRow conn pid
      liftIO $ destroy conn (p :: Post)
      respond $ redirectTo "/admin/posts"

