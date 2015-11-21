{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  create_unique_index "blog_username_idx" "blog" ["username"]

down :: Connection -> IO ()
down = migrate $
  drop_index "blog_username_idx"

main :: IO ()
main = defaultMain up down

