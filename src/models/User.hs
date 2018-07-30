{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module User where

import Model
import Data.Aeson
import GHC.Generics
import Data.Text
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Data.Binary.Builder (toLazyByteString)
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)

data User = User {userId :: Integer,
                  name :: Text,
                  surname :: Text,
                  avatar :: Text,
                  creationTime :: T.LocalTimestamp,
                  isAdmin :: Bool} deriving (Show, Generic)

{-
instance FromRow Present where
  fromRow = Present <$> field

instance FromRow Child where
  fromRow = Child <$> field <*> liftM2 Location field field}
-}

{-
instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "id" <*> v .: "first_name"
  parseJSON _ = mzero
-}

--getLocTimestamp :: B.ByteString -> T.LocalTimestamp
getLocTimestamp = either undefined id . T.parseLocalTimestamp

instance FromJSON T.LocalTimestamp where
  parseJSON (String t) = return $ getLocTimestamp $ encodeUtf8 t
  parseJSON _          = mzero

instance ToJSON T.LocalTimestamp where
  toJSON = String . toStrict . decodeUtf8 . toLazyByteString . T.localTimestampToBuilder

instance FromJSON User where
  parseJSON (Object v) = User (-1) <$> v .: "name" <*> v .: "surname"
    <*> v .: "avatar" <*> (pure $ getLocTimestamp "2017-07-28 14:14:14") <*> pure False --replace default data with maybe
instance ToJSON User

instance Model User where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()

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