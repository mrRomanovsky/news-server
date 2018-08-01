{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Draft where
import qualified Post as P
import qualified User as U
import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Text
import Data.Vector
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import qualified Database.PostgreSQL.Simple.Time as T
{-
CREATE TABLE drafts (
  draft_id SERIAL PRIMARY KEY,
  post_id INTEGER REFERENCES posts,
  creation_time timestamp default current_timestamp,
  category_id INTEGER REFERENCES categories,
  tags text[],
  text_content text NOT NULL,
  main_photo text NOT NULL,
  additional_photos text[],
  post_comments text[]
);
-}

data Draft = Draft { draftId, postId, authorId :: Integer, creationTime :: T.LocalTimestamp
                   , categoryId :: Integer, tags :: Maybe (Vector Text)
                   , textContent :: Text, mainPhoto :: Text
                   , additionalPhotos :: Maybe (Vector Text)} deriving (Show, Generic)

instance FromJSON Draft where
  parseJSON (Object v) = Draft <$> (v .: "draftId" <|> pure (-1)) 
    <*> v .: "postId" <*> v .: "authorId"
    <*> (v .: "creationTime" <|> pure (U.getLocTimestamp "2017-07-28 14:14:14")) <*> v .: "categoryId"
    <*> v .:? "tags" <*> v .: "textContent" <*> v .: "mainPhoto" <*> v .:? "additionalPhotos" 
  parseJSON _ = mzero
instance ToJSON Draft

instance FromRow Draft where
  fromRow = Draft <$> field <*> field <*> field
    <*> field <*> field <*> field <*> field
    <*> field <*> field