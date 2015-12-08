{-# LANGUAGE OverloadedStrings #-}
module Blog.Controllers.PreferencesController where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.HashMap.Strict as H
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.ORM
import Web.Simple
import Web.Simple.Session
import Web.Simple.Templates
import Web.Frank (get, post)

import Blog.Common
import Blog.Models.Blog

preferencesController :: Controller AdminSettings ()
preferencesController = do
  blog <- currentBlog
  csrf <- sessionLookup "csrf_token"

  get "/" $ render "dashboard/preferences.html" $
    object [ "blog" .= blog, "csrf_token" .= fmap decodeUtf8 csrf ]

  post "/" $ do
    params <- fst <$> parseForm
    verifyCSRF params
    let title = decodeUtf8 $ fromMaybe "" $ lookup "title" params
    let newblog = blog { blogTitle = title }
    eres <- withConnection $ \conn ->
      liftIO $ trySave conn newblog
    case eres of
      Left errs -> render "dashboard/preferences.html" $
        object [ "blog" .= newblog, "title_errors" .= errs ]
      Right _ -> redirectBack

  post "/password" $ do
    params <- fst <$> parseForm
    verifyCSRF params
    case validateNewPassword params of
      Left errs -> render "dashboard/preferences.html" $
        object [ "blog" .= blog, "csrf_token" .= fmap decodeUtf8 csrf
               , "password_errors" .= errs ]
      Right passwd -> do
        withConnection $ \conn -> liftIO $ blogChangePassword conn blog passwd
        redirectBack

validateNewPassword :: [(ByteString, ByteString)]
                 -> Either ValidationError ByteString
validateNewPassword params =
  let errs = validatePassword' params
  in if H.null $ validationErrors errs then
    Right $ fromJust $ lookup "new_password" params
    else Left errs
  where validatePassword' =
             validate (\prms ->
               case lookup "new_password" prms of
                Nothing -> False
                Just pwd | pwd == mempty -> False
                _ -> True) "new_password" "New password cannot be empty"
          <> validate (\prms ->
                lookup "verify_password" prms == lookup "new_password" prms)
                "verify_password" "Password verification does not match"

