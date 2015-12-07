{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  add_column "blog" "title" "varchar(255) NOT NULL DEFAULT 'Mostly Typed'"
  conn <- ask
  liftIO $ execute_ conn "alter table \"blog\" alter \"title\" DROP DEFAULT"


down :: Connection -> IO ()
down = migrate $ do
  drop_column "blog" "title"

main :: IO ()
main = defaultMain up down

