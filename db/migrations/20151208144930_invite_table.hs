{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $
  create_table "invite" [ column "invite_code" "text PRIMARY KEY" ]

down :: Connection -> IO ()
down = migrate $ do
  drop_table "invite"

main :: IO ()
main = defaultMain up down

