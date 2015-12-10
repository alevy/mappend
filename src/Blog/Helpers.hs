{-# LANGUAGE OverloadedStrings #-}
module Blog.Helpers where

import Prelude hiding (truncate)

import Data.Aeson
import Data.Default
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List
import Data.Maybe
import Data.Text (Text, replace, unpack)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromChunks, toStrict)
import Data.Time.LocalTime
import Data.Time.Format
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Highlighting.Kate (formatHtmlBlock, defaultFormatOpts, highlightAs)
import qualified Text.Markdown as Markdown
import Text.HTML.Truncate (truncateHtml)
import Web.Simple.Templates

import Blog.Models.Blog (FontInfo(..), Font(..))

helperFunctions :: FunctionMap
helperFunctions = fromList
  [ ("formatTime", toFunction timeFormatter)
  , ("md5", toFunction md5)
  , ("xmlEscape", toFunction xmlEscape)
  , ("zonedToUTC", toFunction zonedToUTC)
  , ("truncate", toFunction truncate)
  , ("fontsUrl", toFunction fontsUrl)]

fontsUrl :: FontInfo -> Value
fontsUrl fi =
  let headerFonts = foldl processFont [] $ fiHeader fi
      fonts = foldl processFont headerFonts $ fiBody fi
  in toJSON $ "//fonts.googleapis.com/css?family=" `T.append` T.intercalate "|" fonts
  where processFont :: [Text] -> Font -> [Text]
        processFont accm font =
          if fontBuiltin font
            then accm
            else (T.replace " " "+" $ fontName font):accm

md5 :: Text -> Value
md5 = toJSON . show . MD5.md5 . encodeUtf8 . fromChunks . (:[])

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
