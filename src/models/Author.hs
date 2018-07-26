{-# LANGUAGE DeriveGeneric #-}

module Author where

import User
import Model
import Data.Aeson
import GHC.Generics
import Data.Text

data Author = Author {user :: User, desc :: Text} deriving (Show, Generic)

instance FromJSON Author
instance ToJSON Author

instance Model Author where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-
Авторы
Ссылка на пользователя (то есть все авторы — пользователи, но не все пользователи — авторы)
Краткое описание
-}