module Blog.Routing.Router where

import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
import Blog.Exceptions.Exceptions
import Network.HTTP.Types (Query, methodGet, methodPost)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Environment

type Router = Request -> Connection -> IO Response

defaultRouter :: Router
defaultRouter _ _ = return notFound

addPostRouter :: (Request -> Bool) -> Router -> Router -> Router
addPostRouter condition = addRouter $ \r -> isPostRequest r && condition r

addGetRouter :: (Request -> Bool) -> Router -> Router -> Router
addGetRouter condition = addRouter $ \r -> isGetRequest r && condition r

isGetRequest :: Request -> Bool
isGetRequest = (== methodGet) . requestMethod

isPostRequest :: Request -> Bool
isPostRequest = (== methodPost) . requestMethod

addRouter :: (Request -> Bool) -> Router -> Router -> Router
addRouter condition newRouter oldRouter request =
  if condition request
    then newRouter request
    else oldRouter request
