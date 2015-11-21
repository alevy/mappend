{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  create_table "blog"
    [ column "id" "serial PRIMARY KEY"
    , column "username" "varchar(255) NOT NULL UNIQUE"
    , column "openid" "varchar(255) UNIQUE" ]
  conn <- ask
  liftIO $ do
    admins <- query_ conn "select openid from admins limit 1"
    case admins of
      (Only openid:_) -> do
        execute conn
          "insert into \"blog\" (id, username, openid) values(DEFAULT, 'admin', ?)"
          (Only openid :: Only String)
        return ()
      _ -> return ()
  drop_table "admins"

down :: Connection -> IO ()
down = migrate $ do
  drop_table "blog"
  create_table "admins"
    [ column "openid" "varchar(255) PRIMARY KEY" ]
  conn <- ask
  liftIO $ execute_ conn
    "insert into \"admins\" (openid) values('http://someopen.id')"

main :: IO ()
main = defaultMain up down

