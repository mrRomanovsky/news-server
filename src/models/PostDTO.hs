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
import Control.Monad
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

insertComment :: Text -> B.ByteString -> Connection -> IO ()
insertComment pNumber comment conn =
  void $ execute conn insertCommentQuery (comment, pNumber)

insertCommentQuery :: Query --maybe I should add dimension (1) as a second parameter to array_length?
insertCommentQuery = "UPDATE posts \
\SET post_comments[array_length(post_comments, 1) + 1] = ? \
\WHERE post_id = ?"

deleteComment :: Text -> B.ByteString -> Connection -> IO ()
deleteComment pNumber cNumber conn = 
  void $ execute conn deleteCommentQuery (cNumber, pNumber)

deleteCommentQuery :: Query
deleteCommentQuery = "UPDATE posts \
\SET post_comments = array_remove(post_comments, post_comments[?]) \
\WHERE post_id = ?"

getPostComments :: Text -> Maybe Page -> Connection -> IO [[Vector Text]]
getPostComments pNumber p conn =
  query conn (paginate postComments p) [pNumber]

postComments :: Query
postComments = "SELECT post_comments FROM posts WHERE post_id = ?"

getPostsBySubstr :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsBySubstr substr p conn = 
  let
    substrRegex = "%" `B.append` substr `B.append` "%"
    in query conn (paginate postsWithSubstr p) (substrRegex, substrRegex, substrRegex)

postsWithSubstr :: Query
postsWithSubstr = "SELECT posts.* FROM (posts JOIN authors \
  \ON posts.author_id = authors.author_id \
  \JOIN users ON authors.users_id = users.users_id) \
  \WHERE (posts.text_content LIKE ?) OR (posts.post_name LIKE ?) \
  \OR (users.users_name LIKE ?) "
  
{-
API новостей должно поддерживать сортировку по:
дате,
автору (имя по алфавиту), 
по категориям (название по алфавиту), 
по количеству фотографий
-}

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

getPostsByCategory :: B.ByteString -> Maybe Page -> Connection -> IO [PostDTO]
getPostsByCategory cat p conn =
  query conn (paginate postsTagsAll p)
    [cat]

postsByAuthor :: Query
postsByAuthor = "SELECT * FROM posts \
\WHERE author_id = (SELECT author_id FROM authors \
  \WHERE users_id = (SELECT users_id FROM users \
    \WHERE users_name = ?))"

postsByCategory :: Query
postsByCategory = "SELECT * FROM posts WHERE category_id = ?"

postsWithSubstrInContent :: Query
postsWithSubstrInContent = "SELECT * FROM posts WHERE text_content LIKE ?"

postsWithSubstrInName :: Query
postsWithSubstrInName = "SELECT * FROM posts WHERE post_name LIKE ?"

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