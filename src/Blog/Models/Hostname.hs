{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Hostname where

import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector)
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple (Connection)

import GHC.Generics

import Blog.Models.Blog

data Hostname = Hostname { hostnameId :: DBKey
                         , hostnameHostname :: Text
                         , hostnameTags :: Vector Text
                         , hostnameBlogId :: DBRef Blog }
                  deriving (Show, Generic)

instance Model Hostname where
  modelInfo = underscoreModelInfo "hostname"

hostnameBlog :: Association Hostname Blog
hostnameBlog = belongsTo

findByHostname :: Connection -> Text -> IO (Maybe Blog)
findByHostname conn hname = fmap listToMaybe $
  dbSelect conn $ addWhere "hostname.hostname = ?" [hname] $
    assocProject hostnameBlog

