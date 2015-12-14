{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  add_column "post" "tags" "text[] NOT NULL DEFAULT '{}'"
  create_index "post_tags_idx" "post" ["blog_id", "tags"]

down :: Connection -> IO ()
down = migrate $ do
  drop_index "post_tags_idx"
  drop_column "post" "tags"

main :: IO ()
main = defaultMain up down

