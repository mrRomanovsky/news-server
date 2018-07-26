{-# LANGUAGE DeriveGeneric #-}

module User where

import Model
import Data.Aeson
import GHC.Generics
import Data.Text
--import qualified Data.ByteString.Lazy as B

data User = User {name :: Text,
                  surname :: Text,
                  avatar :: Text,
                  creation_time :: Text,
                  isAdmin :: Bool} deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

instance Model User where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-
Пользователь
имя
фамилия
аватарка
дата создания
админ (булевая переменная)
-}