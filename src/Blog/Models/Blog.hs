{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Blog where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.ORM
  ( addWhere, modelDBSelect, dbSelect
  , Model(..), DBKey, underscoreModelInfo
  , ValidationError, validate, validateNotEmpty)
import Database.PostgreSQL.Simple (Connection)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import GHC.Generics

data Blog = Blog { blogId :: DBKey
                 , blogUsername :: Text
                 , blogTitle :: Text
                 , blogOpenid :: Text } deriving (Show, Generic)

validateUsername :: Blog -> ValidationError
validateUsername = validate (\blog -> blogUsername blog =~ pattern)
    "username"
    "Username must contain only letters, numbers and dashes"
  where pattern :: Text
        pattern = "^[a-z1-9-]{0,32}$"

instance Model Blog where
  modelInfo = underscoreModelInfo "blog"

  modelValid =
    validateNotEmpty blogUsername
      "username" "Username cannot be empty"
    <> validateUsername
    <> validateNotEmpty blogTitle
      "title" "Title cannot be empty"

instance ToJSON Blog where
    toJSON blog =
      object ["title" .= blogTitle blog, "username" .= blogUsername blog]

findByUsername :: Connection -> Text -> IO (Maybe Blog)
findByUsername conn username = fmap listToMaybe $
  dbSelect conn $ addWhere "username = ?" [username] $ modelDBSelect

findByOpenid :: Connection -> Text -> IO (Maybe Blog)
findByOpenid conn openid = fmap listToMaybe $
  dbSelect conn $ addWhere "openid = ?" [openid] $ modelDBSelect

blogLogin :: Connection -> Text -> Text -> IO (Maybe Blog)
blogLogin conn username password = do
  listToMaybe `fmap` (dbSelect conn $
    addWhere "username = ?" [username] $
    addWhere "password_digest = crypt(?, password_digest)" [password] $
    modelDBSelect)

