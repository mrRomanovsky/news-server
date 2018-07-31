{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Draft where

import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Text
import Database.PostgreSQL.Simple.FromRow

{-
CREATE TABLE drafts (
  draft_id SERIAL PRIMARY KEY,
  post_id INTEGER REFERENCES posts,
  draft_text TEXT NOT NULL
);

-}

data Draft = Draft {draftId, postId :: Integer, draftText :: Text} deriving (Show, Generic)

{-
instance FromJSON Category where
  parseJSON (Object v) = Category <$> (v .: "categoryId" <|> pure (-1)) <*> v .: "name" <*> v .:? "parentId"
  parseJSON _ = mzero
instance ToJSON Category

-}
instance FromJSON Draft where
  parseJSON (Object v) = Draft <$> (v .: "draftId" <|> pure (-1)) <*> v .: "postId" <*> v .: "draftText"
  parseJSON _ = mzero
instance ToJSON Draft

instance FromRow Draft where
  fromRow = Draft <$> field <*> field <*> field