{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module User where

import Model
import Data.Aeson
import GHC.Generics
import Data.Text hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Data.Binary.Builder (toLazyByteString)
import Control.Monad
import Control.Applicative
import DbRequests
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)

data User = User {userId :: UserId,
                  name :: Text,
                  surname :: Text,
                  avatar :: Text,
                  creationTime :: T.LocalTimestamp,
                  isAdmin :: Bool} deriving (Show, Generic)

newtype UserId = UserId {uId :: Integer}

instance Show UserId where
  show = show . uId

instance ToJSON UserId where
  toJSON = toJSON . uId

instance FromJSON UserId where
  parseJSON = fmap UserId . parseJSON

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance ToField UserId where
  toField = toField . uId

getLocTimestamp = either undefined id . T.parseLocalTimestamp

instance FromJSON T.LocalTimestamp where
  parseJSON (String t) = return $ getLocTimestamp $ encodeUtf8 t
  parseJSON _          = mzero

instance ToJSON T.LocalTimestamp where
  toJSON = String . toStrict . decodeUtf8 . toLazyByteString . T.localTimestampToBuilder

instance FromJSON User where
  parseJSON (Object v) = User (UserId (-1)) <$> v .: "name" <*> v .: "surname"
    <*> v .: "avatar" <*> (pure $ getLocTimestamp "2017-07-28 14:14:14") <*> pure False --replace default data with maybe
  parseJSON _ = mzero
instance ToJSON User

instance Model User UserId where
  create User{User.name = n, User.surname = s, User.avatar = a} conn = do
    execute conn "INSERT INTO users(users_name, users_surname, avatar, is_admin) values (?, ?, ?, FALSE)"
      (n, s, a)
    return () --maybe I should do something with execute to remove this "return"

  read = getRecords ("users" :: Text)

  update = error "Sorry, this feature is not implemented yet"

  delete uId conn = do
    execute conn "DELETE FROM drafts WHERE author_id = (SELECT author_id FROM authors WHERE users_id = ?)" [uId]
    execute conn "DELETE FROM posts WHERE author_id = \
                    \(SELECT author_id FROM authors WHERE users_id = ?)" [uId]
    execute conn "DELETE FROM authors WHERE users_id = ?" [uId]
    execute conn "DELETE FROM users WHERE users_id=?" [uId]
    return ()

{-
query conn "SELECT * FROM ?" [Identifier table] -- $ Identifier table
-}

{-
DELETE FROM films USING producers
  WHERE producer_id = producers.id AND producers.name = 'foo'
-}
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
{-
Пользователь
имя
фамилия
аватарка
дата создания
админ (булевая переменная)
-}