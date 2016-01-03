{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Blog.Models.Post where

import Blog.Models.Blog (Blog)

import Data.Aeson
import Data.Char
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple (Connection)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

import GHC.Generics

data Post = Post { postId :: DBKey
                 , postBlogId :: DBRef Blog
                 , postTitle :: Text
                 , postSlug :: Text
                 , postSummary :: Text
                 , postBody :: Text
                 , postBodyHtml :: Text
                 , postPostedAt :: Maybe ZonedTime} deriving (Show, Generic)

getPosts :: Blog -> DBSelect Post
getPosts blog = assocWhere has blog

findPost :: Connection -> Blog -> DBRef Post -> IO (Maybe Post)
findPost conn blog pid = fmap listToMaybe $ dbSelect conn $
  addWhere "post.id = ?" [pid] $ getPosts blog

instance ToJSON Post where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

validateSlug :: Post -> ValidationError
validateSlug = validate (\post -> postSlug post =~ pattern)
    "slug"
    "Slug must contain only letters, numbers and dashes"
  where pattern :: Text
        pattern = "^[a-z0-9-]{0,32}$"

instance Model Post where
  modelInfo = underscoreModelInfo "post"

  modelValid =
    validateNotEmpty postTitle
      "title" "Title cannot be empty"
    <> validateNotEmpty postTitle
      "summary" "Summary cannot be empty"
    <> validateNotEmpty postBody
      "body"  "Body cannot be empty"
    <> validateNotEmpty postSlug
      "slug"  "Slug cannot be empty"
    <> validateSlug

slugFromTitle :: Text -> Text
slugFromTitle title = T.take 32 $
  T.map (\c -> if c == ' ' then '-' else toLower c) $
  T.filter (\c -> c == ' ' || isAlphaNum c) title

summarizePost :: Int -> Text -> Text
summarizePost maxLen originalHtml =
  if T.length result < maxLen - 3
    then result
    else T.append (T.take (maxLen - 3) result) "..."
  where result :: Text
        result = T.replace "\n" " " $ T.take maxLen stripTags

        stripTags :: Text
        stripTags = snd $ T.foldl stripTags' (True, "") originalHtml

        stripTags' :: (Bool, Text) -> Char -> (Bool, Text)
        stripTags' (intxt, accm) char =
          case char of
            '<' | intxt -> (False, accm)
            '>' | not intxt -> (True, accm)
            c   | intxt -> (True, accm `T.snoc` c)
            _ -> (intxt, accm)

