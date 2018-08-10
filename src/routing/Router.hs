module Router where

import Network.HTTP.Types (Query, methodGet, methodPost, status200)
import Network.Wai
import Data.Maybe (fromMaybe)
import System.Environment
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp (run)
import DbRequests

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