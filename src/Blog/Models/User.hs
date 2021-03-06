{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blog.Models.User where

import Blog.Models.Model
import Blog.ServerDB.DbRequests
import Control.Monad
import Data.Aeson
import Data.Binary.Builder (toLazyByteString)
import Data.Text hiding (head)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data User = User
  { userId :: Maybe UserId
  , name :: Text
  , surname :: Text
  , avatar :: Text
  , creationTime :: T.LocalTimestamp
  , isAdmin :: Bool
  } deriving (Show, Generic)

newtype UserId = UserId
  { uId :: Integer
  }

instance Show UserId where
  show = show . uId

instance ToJSON UserId where
  toJSON = toJSON . uId

instance FromJSON UserId where
  parseJSON = fmap UserId . parseJSON

instance FromField UserId where
  fromField field mdata = UserId <$> fromField field mdata

instance ToField UserId where
  toField = toField . uId

getLocTimestamp = either undefined id . T.parseLocalTimestamp

instance FromJSON T.LocalTimestamp where
  parseJSON (String t) = return $ getLocTimestamp $ encodeUtf8 t
  parseJSON _ = mzero

instance ToJSON T.LocalTimestamp where
  toJSON =
    String .
    toStrict . decodeUtf8 . toLazyByteString . T.localTimestampToBuilder

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .:? "userId" <*> v .: "name" <*> v .: "surname" <*> v .: "avatar" <*>
    pure (getLocTimestamp "2017-07-28 14:14:14") <*>
    pure False
  parseJSON _ = mzero

instance ToJSON User

instance Model User UserId where
  create User {Blog.Models.User.name = n, surname = s, avatar = a} conn =
    void $
    execute
      conn
      "INSERT INTO users(\"user_name\", user_surname, user_avatar, user_is_admin) values (?, ?, ?, FALSE)"
      (n, s, a)
  getData = getRecords ("users" :: Text)
  update = error "Sorry, this feature is not implemented yet"
  delete uId conn =
    void $ execute conn "DELETE FROM users WHERE \"user_id\" = ?" [uId]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
