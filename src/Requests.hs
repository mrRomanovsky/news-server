module Requests where

import Post
import User
import Author
import Category
import Data.Text
import Network.Wai
import qualified Data.ByteString.Lazy as B

----type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

data UsersRequest = CreateUser User | GetUsers

data AuthorsRequest = CreateAuthor Author | GetAuthors | UpdateAuthor Author Author

data CategoriesRequest = CreateCategory Category | GetCategories | UpdateCategory Category Category

data PostsRequest = GetPosts | GetPostsBy (Post -> Bool)

data CommentsRequest = GetCommentsForPost Post | AddCommentForPost Post B.ByteString | DeleteCommentForPost Post Int

--data TagsRequest = CreateTag Tag | UpdateTag Tag Tag | 