{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  add_column "blog" "theme" "jsonb"

down :: Connection -> IO ()
down = migrate $ do
  drop_column "blog" "theme"

main :: IO ()
main = defaultMain up down

