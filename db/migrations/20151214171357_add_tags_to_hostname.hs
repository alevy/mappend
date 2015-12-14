{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  add_column "hostname" "tags" "text[] NOT NULL DEFAULT '{}'"

down :: Connection -> IO ()
down = migrate $ do
  drop_column "hostname" "tags"

main :: IO ()
main = defaultMain up down

