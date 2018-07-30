{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Tag where

import Model
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

data Tag = Tag {tagId :: Integer, tagName :: Text} deriving (Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

{-instance FromField Text where
  fromField f Nothing = returnError ConversionFailed f "Nothing returned for A"
  fromField f (Just bs) = return $ decodeUtf8 bs-}

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

instance Model Tag where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()