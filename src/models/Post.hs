module Post where

import Author
import Category
import Tag
import Model
import qualified Data.ByteString.Lazy as B

data Post = Post { name :: B.ByteString, creation_date :: B.ByteString
                 , author :: Author, category :: Category, tags :: [Tag]
                 , text :: B.ByteString, mainPhoto :: B.ByteString
                 , additionalPhotos :: [B.ByteString]
                 , comments :: [B.ByteString]}

instance Model Post where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-Новости:
краткое название
дата создания
один автор
одна категория
множество тегов
текстовый контент
одна главная фотография
множество дополнительных фотографий-}