{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  drop_column "blog" "openid"

down :: Connection -> IO ()
down = migrate $ do
  add_column "blog" "openid" "text UNIQUE"

main :: IO ()
main = defaultMain up down

