{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Maybe
import Data.Int
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple

up :: Connection -> IO ()
up = migrate $ do
  conn <- ask
  mblogid <- liftIO $ listToMaybe <$>
    query_ conn "select id from \"blog\" limit 1"
  case mblogid of
    Just (Only blogid) -> do
      liftIO $ do
        execute conn
          "alter table \"post\" add \"blog_id\" integer references \"blog\"(id) NOT NULL DEFAULT ?"
          (Only blogid :: Only Int64)
        execute_ conn "alter table \"post\" alter \"blog_id\" DROP DEFAULT"
        return ()
    Nothing -> do
      add_column "post" "blog_id" "integer REFERENCES \"blog\"(id) NOT NULL"
      return ()

down :: Connection -> IO ()
down = migrate $ do
  drop_column "post" "blog_id"

main :: IO ()
main = defaultMain up down

