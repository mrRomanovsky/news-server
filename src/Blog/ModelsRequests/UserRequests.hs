module Blog.ModelsRequests.UserRequests
  ( getUsers
  , createUser
  , deleteUser
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Blog.Models.Model
import Network.Wai
import Prelude hiding (read)
import Blog.ModelsRequests.RequestsUtils
import qualified Blog.Models.User as U

getUsers :: Request -> Connection -> IO Response
getUsers request =
  let (page, sortBy, _) = getAdditionalParams request
   in fmap respondJson . (read page sortBy :: Connection -> IO [U.User])

createUser :: Request -> Connection -> IO Response
createUser request = actionWithBody request createUserBs

createUserBs :: B.ByteString -> Connection -> IO Response
createUserBs = createModel . decodeUser

deleteUser :: Request -> Connection -> IO Response
deleteUser request =
  authorizeRequest request $ actionWithBody request deleteUserBs

deleteUserBs :: B.ByteString -> Connection -> IO Response
deleteUserBs =
  deleteModel . (eitherDecode :: B.ByteString -> Either String U.UserId)
