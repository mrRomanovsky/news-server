{-# LANGUAGE DeriveGeneric #-}

module Post where

import Author
import Category
import Tag
import Model
import Data.Aeson
import GHC.Generics
import Data.Text

data Post = Post { name :: Text, creation_date :: Text
                 , author :: Author, category :: Category, tags :: [Tag]
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: [Text]
                 , comments :: [Text]} deriving (Show, Generic)

instance Model Post where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()

instance FromJSON Post
instance ToJSON Post
{-Новости:
краткое название
дата создания
один автор
одна категория
множество тегов
текстовый контент
одна главная фотография
множество дополнительных фотографий-}