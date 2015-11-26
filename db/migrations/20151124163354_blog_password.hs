{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  conn <- ask
  liftIO $ execute_ conn "create extension if not exists pgcrypto"
  add_column "blog" "password_digest"
    "text NOT NULL DEFAULT crypt(md5(random()::text), gen_salt('bf'))"

down :: Connection -> IO ()
down = migrate $ do
  drop_column "blog" "password_digest"

  conn <- ask
  liftIO $ execute_ conn "drop extension if exists pgcrypto"

main :: IO ()
main = defaultMain up down

