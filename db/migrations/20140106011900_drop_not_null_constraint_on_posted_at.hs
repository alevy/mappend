{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up c = (flip migrate) c $ do
  change_column "post" "posted_at" "drop not null"
  liftIO $ execute_ c "update post set posted_at = null where not published"
  drop_column "post" "published"
  

down :: Connection -> IO ()
down c = (flip migrate) c $ do
  add_column "post" "published" "boolean not null default true"
  liftIO $ do
    execute_ c "update post set published = false, posted_at = now() where posted_at is null"
  change_column "post" "posted_at" "set not null"

main :: IO ()
main = defaultMain up down

