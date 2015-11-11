{-# LANGUAGE OverloadedStrings #-}
module Blog.Helpers where

import Prelude hiding (truncate)

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text, unpack, pack, replace)
import Data.Time.LocalTime
import Data.Time.Format
import Network.Gravatar
import Text.HTML.Truncate (truncateHtml)
import Text.Pandoc (writeHtmlString, readMarkdown, writerHighlight)
import Text.Pandoc.Error
--import System.Locale
import Web.Simple.Templates

helperFunctions :: FunctionMap
helperFunctions = fromList
  [ ("formatTime", toFunction timeFormatter)
  , ("gravatar", toFunction gravatarUrl)
  , ("xmlEscape", toFunction xmlEscape)
  , ("zonedToUTC", toFunction zonedToUTC)
  , ("truncate", toFunction truncate)]

gravatarUrl :: Text -> Maybe Int -> Value
gravatarUrl email size = toJSON $
  gravatar (def {gSize = fmap Size size, gDefault = Just Wavatar}) email

timeFormatter :: ZonedTime -> Maybe String -> Value
timeFormatter t mfmt =
  let fmt = fromMaybe "%B %e, %C%y %l:%M%P" mfmt
  in toJSON $ formatTime defaultTimeLocale fmt t

markdown :: Text -> Text
markdown = pack . (writeHtmlString (def { writerHighlight = True}))
                . handleError . (readMarkdown def)
                . (filter (/= '\r')) . unpack

truncate :: Int -> Text -> Value
truncate len body = toJSON $ truncateHtml len body

xmlEscape :: Text -> Value
xmlEscape raw = toJSON $ foldl' escapr (replace "&" "&amp;" raw) escapeMap
  where escapeMap = [ (">", "&gt;")
                    , ("<", "&lt;")
                    , ("'", "&apos;")
                    , ("\"", "&quot;")]
        escapr txt (orig, to) = replace orig to txt

zonedToUTC :: ZonedTime -> Value
zonedToUTC = toJSON . zonedTimeToUTC
