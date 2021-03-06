module Blog.Handlers.User
  ( getUsers
  , createUser
  , deleteUser
  ) where

import Blog.Handlers.HandlersUtils
import Blog.Models.Model
import qualified Blog.Models.User as U
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Network.Wai
import Prelude hiding (read)

getUsers :: Request -> Connection -> IO Response
getUsers request =
  let (page, sortBy, _) = getAdditionalParams request
   in fmap respondJson . (getData page sortBy :: Connection -> IO [U.User])

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
