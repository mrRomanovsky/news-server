{-# LANGUAGE DeriveGeneric #-}

module Author where

import User
import Model
import Data.Aeson
import GHC.Generics
import Data.Text
import Database.PostgreSQL.Simple.FromRow

data Author = Author {authorId, userId :: Integer, desc :: Text} deriving (Show, Generic)

instance FromJSON Author
instance ToJSON Author

instance Model Author where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field
{-
Авторы
Ссылка на пользователя (то есть все авторы — пользователи, но не все пользователи — авторы)
Краткое описание
-}