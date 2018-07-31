{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Author where

import User
import Model
import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Text
import Database.PostgreSQL.Simple.FromRow

data Author = Author {authorId, userId :: Integer, desc :: Maybe Text} deriving (Show, Generic)

{-
instance FromJSON Category where
  parseJSON (Object v) = Category <$> (v .: "categoryId" <|> pure (-1)) <*> v .: "name" <*> v .:? "parentId"
  parseJSON _ = mzero
instance ToJSON Category

-}
instance FromJSON Author where
  parseJSON (Object v) = Author <$> (v .: "authorId" <|> pure (-1)) <*> v .: "userId" <*> v .:? "desc"
  parseJSON _ = mzero
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