{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Post where

import Control.Applicative
import Prelude hiding (takeWhile)
import Author
import User
import Category
import Tag
import Model
import Data.Aeson
import GHC.Generics
import Data.Text hiding (takeWhile)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Time as T

data Post = Post { postId :: Integer, postName :: Text, creation_date :: T.LocalTimestamp
                 , authorId :: Integer, categoryId :: Integer, tags :: Maybe [Text]
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: Maybe [Text]
                 , comments :: Maybe [Text]} deriving (Show, Generic)

instance FromField ([] Text) where
  fromField f Nothing = returnError ConversionFailed f "No list returned!"
  fromField f (Just bs) =
    case parseOnly textList (decodeUtf8 bs) of
      Left err -> returnError ConversionFailed f err
      Right tags -> return tags

textList :: Parser [Text]
textList = do
  char '{'
  many1 textComma
 -- char '}'
 -- return elems

textComma :: Parser Text
textComma = do
  tag <- takeWhile (\c -> c /= ',' && c /= '}')
  char ',' <|> char '}'
  return tag

instance Model Post where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()

instance FromJSON Post
instance ToJSON Post

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
{-Новости:
краткое название
дата создания
один автор
одна категория
множество тегов
текстовый контент
одна главная фотография
множество дополнительных фотографий-}