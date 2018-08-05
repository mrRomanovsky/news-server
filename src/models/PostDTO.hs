{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module PostDTO where

import Control.Applicative
import Prelude hiding (takeWhile)
import Author
import User
import Category
import Tag
import Model
import DbRequests
import Data.Aeson
import GHC.Generics
import Data.String (fromString)
import Data.Text hiding (takeWhile)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Data.ByteString as B
import Data.Vector hiding ((++))
import qualified Database.PostgreSQL.Simple.Time as T

data PostDTO = PostDTO { postId :: PostId, postName :: Text, creation_date :: T.LocalTimestamp
                 , authorId :: Integer, categoryId :: Integer, tags :: Maybe (Vector Integer)
                 , text :: Text, mainPhoto :: Text
                 , additionalPhotos :: Maybe (Vector Text)
                 , comments :: Maybe (Vector Text)} deriving (Show, Generic)

newtype PostId = PostId {pId :: Integer}

instance Show PostId where
  show = show . pId

instance ToJSON PostId where
  toJSON = toJSON . pId

instance FromField PostId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ PostId x

instance ToField PostId where
  toField = toField . pId

instance Model PostDTO PostId where
  create = error "Sorry, this feature is not implemented yet"

  read = getRecords "posts"

  update = error "Sorry, this feature is not implemented yet"

  delete = error "Sorry, this feature is not implemented yet"

--instance FromJSON PostDTO - not needed yet
instance ToJSON PostDTO

instance FromRow PostDTO where
  fromRow = PostDTO <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

getPostsByAuthor :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsByAuthor author p conn =
  query conn (paginate postsByAuthor p) [author]

getPostsWithSubstrInContent :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithSubstrInContent substr p conn =
  query conn (paginate postsWithSubstrInContent p)
    ["%" `B.append` substr `B.append` "%"]

getPostsWithSubstrInName :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithSubstrInName substr p conn =
  query conn (paginate postsWithSubstrInName p)
    ["%" `B.append` substr `B.append` "%"]

getPostsWithTag :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithTag tag p conn =
  query conn (paginate postsWithTag p)
    ["{" `B.append` tag `B.append` "}"]

getPostsTagsIn :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsTagsIn tagsIn p conn = 
  query conn (paginate postsTagsIn p)
    ["{" `B.append` getArr tagsIn `B.append` "}"]

getPostsTagsAll :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsTagsAll tagsAll p conn =
  query conn (paginate postsTagsAll p)
    ["{" `B.append` getArr tagsAll `B.append` "}"]

postsByAuthor :: Query
postsByAuthor = "SELECT * FROM posts \
\WHERE author_id = (SELECT author_id FROM authors \
  \WHERE users_id = (SELECT users_id FROM users \
    \WHERE users_name = ?))"

postsWithSubstrInContent :: Query
postsWithSubstrInContent = "SELECT * FROM posts WHERE text_content LIKE ?"

postsWithSubstrInName :: Query
postsWithSubstrInName = "SELECT * FROM posts WHERE PostDTO_name LIKE ?"

postsWithTag :: Query
postsWithTag = "SELECT * FROM posts WHERE tags @> ?"

postsTagsIn :: Query
postsTagsIn = "SELECT * FROM posts WHERE tags && ?"

postsTagsAll :: Query
postsTagsAll = "SELECT * FROM posts WHERE tags @> ?"

getArr :: B.ByteString -> B.ByteString
getArr = B.init . B.tail

getPostsDate :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDate date p conn =
  query conn (paginate postsDate p)
    [date]

getPostsDateLt :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDateLt dateLt p conn = 
  query conn (paginate postsDateLt p)
    [dateLt]

getPostsDateGt :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDateGt dateGt p conn =
  query conn (paginate postsDateGt p)
    [dateGt]


paginate :: Query -> Maybe Page -> Query
paginate q p =
  let offset = maybe 0 ((*20) . (subtract 1)) p
      qStr = Prelude.init $ Prelude.tail $ show q
      in fromString $ qStr ++
        "OFFSET " ++ show offset ++ " LIMIT 20"

postsDate :: Query
postsDate = "SELECT * FROM posts WHERE DATE(creation_time) = DATE ?"

postsDateGt :: Query
postsDateGt = "SELECT * FROM posts WHERE DATE(creation_time) > DATE ?"

postsDateLt :: Query
postsDateLt = "SELECT * FROM posts WHERE DATE(creation_time) < DATE ?"