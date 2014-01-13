{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.Migrations
import Database.PostgreSQL.Simple
import Blog.Helpers

up :: Connection -> IO ()
up conn = (flip migrate) conn $ do
  add_column "post" "body_html" "text"
  liftIO $ forEach_ conn "select id,body from post" $ \(body, pid) -> void $
    execute conn "update post set body_html = ? where id = ?"
      (markdown body, pid :: Int)
  change_column "post" "body_html" "set NOT NULL"


down :: Connection -> IO ()
down = migrate $ do
  drop_column "post" "body_html"

main :: IO ()
main = defaultMain up down

