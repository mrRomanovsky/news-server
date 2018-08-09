{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module PostDTO where

import Author
import Category
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import Data.String (fromString)
import Data.Text hiding (takeWhile)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector hiding ((++))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Time as T
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import DbRequests
import GHC.Generics
import Model
import Prelude hiding (takeWhile)
import Tag
import User

data PostDTO = PostDTO
  { postId :: PostId
  , postName :: Text
  , creation_date :: T.LocalTimestamp
  , authorId :: Integer
  , categoryId :: Integer
  , tags :: Maybe (Vector Integer)
  , text :: Text
  , mainPhoto :: Text
  , additionalPhotos :: Maybe (Vector Text)
  , comments :: Maybe (Vector Text)
  } deriving (Show, Generic)

newtype PostId = PostId
  { pId :: Integer
  }

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

instance ToJSON PostDTO

instance FromRow PostDTO where
  fromRow =
    PostDTO <$> field <*> field <*> field <*> field <*> field <*> field <*>
    field <*>
    field <*>
    field <*>
    field

insertComment :: Text -> B.ByteString -> Connection -> IO ()
insertComment pNumber comment conn =
  void $ execute conn insertCommentQuery (comment, pNumber)

insertCommentQuery :: Query
insertCommentQuery =
  "UPDATE posts \
\SET post_comments[array_length(post_comments, 1) + 1] = ? \
\WHERE post_id = ?"

deleteComment :: Text -> B.ByteString -> Connection -> IO ()
deleteComment pNumber cNumber conn =
  void $ execute conn deleteCommentQuery (cNumber, pNumber)

deleteCommentQuery :: Query
deleteCommentQuery =
  "UPDATE posts \
\SET post_comments = array_remove(post_comments, post_comments[?]) \
\WHERE post_id = ?"

getPostComments :: Text -> Maybe Page -> Connection -> IO [[Vector Text]]
getPostComments pNumber = paginatedQuery postComments [pNumber]

postComments :: Query
postComments = "SELECT post_comments FROM posts WHERE post_id = ?"

getPostsBySubstr :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsBySubstr substr =
  let substrRegex = "%" `B.append` substr `B.append` "%"
   in paginatedQuery postsWithSubstr (substrRegex, substrRegex, substrRegex)

postsWithSubstr :: Query
postsWithSubstr =
  "SELECT posts.* FROM (posts JOIN authors \
  \ON posts.author_id = authors.author_id \
  \JOIN users ON authors.user_id = users.user_id) \
  \WHERE (posts.post_text_content LIKE ?) OR (posts.post_name LIKE ?) \
  \OR (users.user_name LIKE ?) "

getPostsSorted :: Text -> Maybe Integer -> Connection -> IO [PostDTO]
getPostsSorted sortParam = paginatedQuery postsSorted [Identifier sortParam]

postsSorted :: Query
postsSorted = "SELECT * FROM posts ORDER BY ? "

getPostsByAuthor :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsByAuthor author = paginatedQuery postsByAuthor [author]

getPostsWithSubstrInContent ::
     B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithSubstrInContent substr =
  paginatedQuery postsWithSubstrInContent ["%" `B.append` substr `B.append` "%"]

getPostsWithSubstrInName ::
     B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithSubstrInName substr =
  paginatedQuery postsWithSubstrInName ["%" `B.append` substr `B.append` "%"]

getPostsWithTag :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsWithTag tag =
  paginatedQuery postsWithTag ["{" `B.append` tag `B.append` "}"]

getPostsTagsIn :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsTagsIn tagsIn =
  paginatedQuery postsTagsIn ["{" `B.append` getArr tagsIn `B.append` "}"]

getPostsTagsAll :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsTagsAll tagsAll =
  paginatedQuery postsTagsAll ["{" `B.append` getArr tagsAll `B.append` "}"]

getPostsByCategory :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsByCategory cat = paginatedQuery postsTagsAll [cat]

postsByAuthor :: Query
postsByAuthor =
  "SELECT * FROM posts \
\WHERE author_id = (SELECT author_id FROM authors \
  \WHERE \"user_id\" = (SELECT \"user_id\" FROM users \
    \WHERE user_name = ?))"

postsByCategory :: Query
postsByCategory = "SELECT * FROM posts WHERE category_id = ?"

postsWithSubstrInContent :: Query
postsWithSubstrInContent = "SELECT * FROM posts WHERE post_text_content LIKE ?"

postsWithSubstrInName :: Query
postsWithSubstrInName = "SELECT * FROM posts WHERE post_name LIKE ?"

postsWithTag :: Query
postsWithTag = "SELECT * FROM posts WHERE post_tags @> ?"

postsTagsIn :: Query
postsTagsIn = "SELECT * FROM posts WHERE post_tags && ?"

postsTagsAll :: Query
postsTagsAll = "SELECT * FROM posts WHERE post_tags @> ?"

getArr :: B.ByteString -> B.ByteString
getArr = B.init . B.tail

getPostsDate :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDate date = paginatedQuery postsDate [date]

getPostsDateLt :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDateLt dateLt = paginatedQuery postsDateLt [dateLt]

getPostsDateGt :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsDateGt dateGt = paginatedQuery postsDateGt [dateGt]

postsDate :: Query
postsDate = "SELECT * FROM posts WHERE DATE(post_creation_time) = DATE ?"

postsDateGt :: Query
postsDateGt = "SELECT * FROM posts WHERE DATE(post_creation_time) > DATE ?"

postsDateLt :: Query
postsDateLt = "SELECT * FROM posts WHERE DATE(post_creation_time) < DATE ?"
