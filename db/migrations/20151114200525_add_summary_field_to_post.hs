{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  add_column "post" "summary" "text"
  conn <- ask
  liftIO $ execute_ conn "update post set summary = title"
  change_column "post" "summary" "SET NOT NULL"


down :: Connection -> IO ()
down = migrate $ do
  drop_column "post" "summary"

main :: IO ()
main = defaultMain up down

