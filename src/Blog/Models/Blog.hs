{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Blog.Models.Blog where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Aeson
  (ToJSON(..), object, (.=), FromJSON(..), (.:?), (.:), withObject, Value)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.ORM
  ( addWhere, modelDBSelect, dbSelect
  , Model(..), DBKey(..), underscoreModelInfo, trySave
  , ValidationError, validateNotEmpty, validationError)
import Database.PostgreSQL.Simple (Connection, execute, query)
import Database.PostgreSQL.Simple.Errors
  (catchViolation,  ConstraintViolation(..))
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import GHC.Generics

data Font = Font { fontName :: Text , fontBuiltin :: Bool }
  deriving (Show, Generic)

instance ToJSON Font where
  toJSON Font{..} = object [ "name" .= fontName, "builtin" .= fontBuiltin ]

instance FromJSON Font where
  parseJSON = withObject "Font" $ \v -> do
    name <- v .: "name"
    builtin <- v .: "builtin"
    return Font { fontName = name, fontBuiltin = builtin }

data FontInfo = FontInfo { fiHeader :: [Font], fiBody :: [Font] }
  deriving (Show, Generic)

instance ToJSON FontInfo where
  toJSON FontInfo{..} = object [ "header" .= fiHeader, "body" .= fiBody ]

instance FromJSON FontInfo where
  parseJSON = withObject "FontInfo" $ \v -> do
    header <- v .: "header"
    body <- v .: "body"
    return FontInfo { fiHeader = header, fiBody = body }

data ColorInfo = ColorInfo { ciPrimary :: Text, ciAccent :: Text }
  deriving (Show, Generic)

instance ToJSON ColorInfo where
  toJSON ColorInfo{..} = object [ "primary" .= ciPrimary, "accent" .= ciAccent ]

instance FromJSON ColorInfo where
  parseJSON = withObject "ColorInfo" $ \v -> do
    primary <- v .: "primary"
    accent <- v .: "accent"
    return ColorInfo { ciPrimary = primary, ciAccent = accent }

data ThemeInfo = ThemeInfo
  { tiName :: Maybe Text
  , tiFonts :: Maybe FontInfo
  , tiColors :: Maybe ColorInfo } deriving (Show, Generic)

instance ToJSON ThemeInfo where
  toJSON ThemeInfo{..} = object
    [ "name" .= tiName , "fonts" .= tiFonts, "colors" .= tiColors ]

instance FromJSON ThemeInfo where
  parseJSON = withObject "ThemeInfo" $ \v -> do
    name <- v .:? "name"
    fonts <- v .:? "fonts"
    colors <- v .:? "colors"
    return $ ThemeInfo { tiName = name, tiFonts = fonts, tiColors = colors }

instance ToField ThemeInfo where
  toField = toField . toJSON

instance FromField ThemeInfo where
  fromField = fromJSONField

data Password = Digest S8.ByteString | Plaintext Text deriving (Generic)

instance Show Password where
  show (Digest pwd) = "Digest " ++ (S8.unpack pwd)
  show _ = "Password ***"

instance ToField Password where
  toField (Digest pwd) = toField pwd
  -- Generates "crypt('mypassword', gen_salt('bf'))"
  toField (Plaintext pwd) = Many [ Plain (byteString "crypt(")
                                 , toField pwd
                                 , Plain (byteString ", gen_salt('bf'))")]

instance FromField Password where
  fromField f dat = Digest <$> fromField f dat

data Blog = Blog { blogId :: DBKey
                 , blogUsername :: Text
                 , blogPasswordDigest :: Password
                 , blogTitle :: Text
                 , blogTheme :: Maybe Value } deriving (Show, Generic)

validatePassword :: Blog -> ValidationError
validatePassword Blog{blogPasswordDigest = Plaintext pwd} | pwd == mempty =
    validationError "password" "Password cannot be empty"
validatePassword _ = mempty

validateUsername :: Blog -> ValidationError
validateUsername Blog{blogUsername = username} =
  if (not $ username =~ pattern)
    then validationError "username"
          "Username must contain only letters, numbers and dashes"
    else mempty
  where pattern :: Text
        pattern = "^[a-z1-9-]{0,32}$"

instance Model Blog where
  modelInfo = underscoreModelInfo "blog"

  modelValid =
    validateNotEmpty blogUsername
      "username" "Username cannot be empty"
    <> validateUsername
    <> validatePassword
    <> validateNotEmpty blogTitle
      "title" "Title cannot be empty"

instance ToJSON Blog where
    toJSON Blog{..} = object
      ["title" .= blogTitle, "username" .= blogUsername, "theme" .= blogTheme]

findByUsername :: Connection -> Text -> IO (Maybe Blog)
findByUsername conn username = fmap listToMaybe $
  dbSelect conn $ addWhere "username = ?" [username] $ modelDBSelect

blogLogin :: Connection -> Text -> Text -> IO (Maybe Blog)
blogLogin conn username password = do
  listToMaybe `fmap` (dbSelect conn $
    addWhere "username = ?" [username] $
    addWhere "password_digest = crypt(?, password_digest)" [password] $
    modelDBSelect)

blogChangePassword :: Connection -> Blog -> ByteString -> IO ()
blogChangePassword conn blog password = do
  execute conn
    "UPDATE \"blog\" SET password_digest = crypt(?, gen_salt('bf')) WHERE id = ?"
    (password, blogId blog)
  return ()

tryRegisterBlog :: Connection -> Text -> Text -> Text -> Text
                -> IO (Either ValidationError Blog)
tryRegisterBlog conn username password verifyPassword inviteCode =
  runExceptT $ do
    inviteExists <- liftIO $ do
      res <- query conn "select invite_code from invite where invite_code = ?"
               [inviteCode]
      return $ not $ null $ (res :: [[Text]])
    when (not inviteExists) $ throwE $
      validationError "invite_code" "You need a valid invite code to register"

    when (password /= verifyPassword) $ throwE $
      validationError "verify_password"
                      "Password verification doesn't match password"

    ExceptT $ catchSqlErr $
      trySave conn $
        Blog { blogId = NullKey
             , blogUsername = username
             , blogPasswordDigest = Plaintext password
             , blogTitle = "My Blog"
             , blogTheme = Nothing }
  where catchSqlErr fn = catchViolation catcher fn
        catcher _ (UniqueViolation "blog_username_key") = return $ Left $
          validationError "username" "Username already exists"
        catcher e _ = throwIO e

