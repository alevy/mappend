{-# LANGUAGE OverloadedStrings #-}
module Blog.Helpers where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text, unpack, replace)
import Data.Time.LocalTime
import Data.Time.Format
import Network.Gravatar
import Text.Pandoc (writeHtmlString, readMarkdown, writerHighlight)
import System.Locale
import Web.Simple.Templates

helperFunctions :: FunctionMap
helperFunctions = fromList
  [ ("formatTime", toFunction timeFormatter)
  , ("gravatar", toFunction gravatarUrl)
  , ("markdown", toFunction markdown)
  , ("xmlEscape", toFunction xmlEscape)
  , ("zonedToUTC", toFunction zonedToUTC)]

gravatarUrl :: Text -> Maybe Int -> Value
gravatarUrl email size = toJSON $
  gravatar (def {gSize = fmap Size size, gDefault = Just Wavatar}) email

timeFormatter :: ZonedTime -> Maybe String -> Value
timeFormatter t mfmt =
  let fmt = fromMaybe "%B %e, %C%y %l:%M%P" mfmt
  in toJSON $ formatTime defaultTimeLocale fmt t

markdown :: Text -> Value
markdown = toJSON . (writeHtmlString (def { writerHighlight = True})) . (readMarkdown def)
               . (filter (/= '\r')) . unpack

xmlEscape :: Text -> Value
xmlEscape raw = toJSON $ foldl' escapr (replace "&" "&amp;" raw) escapeMap
  where escapeMap = [ (">", "&gt;")
                    , ("<", "&lt;")
                    , ("'", "&apos;")
                    , ("\"", "&quot;")]
        escapr txt (orig, to) = replace orig to txt

zonedToUTC :: ZonedTime -> Value
zonedToUTC = toJSON . zonedTimeToUTC
