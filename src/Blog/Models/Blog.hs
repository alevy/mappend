{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Blog where

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

findByUsername :: Connection -> Text -> IO (Maybe Blog)
findByUsername conn username = fmap listToMaybe $
  dbSelect conn $ addWhere "username = ?" [username] $ modelDBSelect

