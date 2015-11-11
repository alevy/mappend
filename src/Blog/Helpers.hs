{-# LANGUAGE OverloadedStrings #-}
module Blog.Helpers where

import Prelude hiding (truncate)

import Data.Aeson
import Data.Default
import Data.List
import Data.Maybe
import Data.Text (Text, replace, unpack)
import Data.Text.Lazy (fromChunks, toStrict)
import Data.Time.LocalTime
import Data.Time.Format
import Network.Gravatar
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Highlighting.Kate (formatHtmlBlock, defaultFormatOpts, highlightAs)
import qualified Text.Markdown as Markdown
import Text.HTML.Truncate (truncateHtml)
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

markdownSettings :: Markdown.MarkdownSettings
markdownSettings =
  let renderer lang (src,_) = formatHtmlBlock defaultFormatOpts $
                                highlightAs (maybe "text" unpack lang) $
                                unpack src
  in def { Markdown.msBlockCodeRenderer = renderer }

markdown :: Text -> Text
markdown = toStrict . renderHtml
         . (Markdown.markdown markdownSettings) . fromChunks . (:[])

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
