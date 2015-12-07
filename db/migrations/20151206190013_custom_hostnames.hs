{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  create_table "hostname"
  [ column "id" "serial PRIMARY KEY"
  , column "hostname" "text NOT NULL UNIQUE"
  , column "blog_id" "integer references blog(id)"]

down :: Connection -> IO ()
down = migrate $ do
  drop_table "hostname"

main :: IO ()
main = defaultMain up down

