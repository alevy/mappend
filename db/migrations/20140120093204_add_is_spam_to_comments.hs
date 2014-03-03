{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  add_column "comment" "is_spam" "boolean NOT NULL DEFAULT false"

down :: Connection -> IO ()
down = migrate $ do
  drop_column "comment" "is_spam"

main :: IO ()
main = defaultMain up down

