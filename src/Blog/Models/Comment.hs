{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}
module Blog.Models.Comment where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as S
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.LocalTime
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.ORM
import Network.HTTP.Client as H
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Web.Simple

import GHC.Generics

import Blog.Common
import Blog.Models.Post

data Comment = Comment { commentId :: DBKey
                       , commentName :: Text
                       , commentEmail :: Text
                       , commentComment :: Text
                       , commentPostId :: DBRef Post
                       , commentCommentedAt :: ZonedTime
                       , commentIsSpam :: Bool }
                  deriving (Show, Generic)

instance ToJSON Comment where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

validateEmail :: Comment -> ValidationError
validateEmail = validate (\comment -> commentEmail comment =~ pattern)
    "email" "You must enter a valid e-mail address"
  where pattern :: Text
        pattern = "^[^@]+[@][^@]+$"

commentFromForm :: [(S.ByteString, S.ByteString)]
                -> DBRef Post -> ZonedTime -> Comment
commentFromForm params pid curTime = Comment NullKey
  (param "name") (param "email") (param "comment")
  pid curTime False
  where param key = decodeUtf8 $ fromMaybe "" $ lookup key params

instance Model Comment where
  modelInfo = underscoreModelInfo "comment"

  modelValid =
    validateNotEmpty commentName
      "name" "Name cannot be empty"
    <> validateEmail
    <> validateNotEmpty commentComment
      "comment" "Comment cannot be empty"

blogspamUrl :: H.Request
blogspamUrl = url { H.method = "POST" }
  where url = fromJust $ H.parseUrl "http://test.blogspam.net:9999/"

data BlogSpamResult = BlogSpamResult { result :: Text, reason :: Maybe Text } deriving (Generic, Show)

instance FromJSON BlogSpamResult

testSpam :: Comment -> Controller AppSettings Comment
testSpam comment = do
  mgr <- httpManager
  rIp <- remoteIp
  req <- request
  let requestObject = encode $ object
                        [ "comment" .= commentComment comment
                        , "ip" .= decodeUtf8 rIp
                        , "site" .= ((mappend "http://") `fmap` decodeUtf8 `fmap` (requestHeaderHost req))
                        , "whitelist" .= ("127.0.0.1" :: Text) ]
      spamReq = blogspamUrl { H.requestBody = H.RequestBodyLBS $ requestObject }
  mres <- liftIO $ decode <$> H.responseBody <$> H.httpLbs spamReq mgr
  case mres of
    Nothing -> fail "couldn't decode response"
    Just res | result res == "OK" -> return comment
             | result res == "ERROR" -> fail . show $ reason res
             | otherwise -> do
                liftIO $ print res
                return $ comment { commentIsSpam = True }

