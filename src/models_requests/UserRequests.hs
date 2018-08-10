module UserRequests where

import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (filter, head)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple hiding (Query)
import DbRequests
import qualified Draft as D
import Model
import Network.HTTP.Types (Query, hAuthorization, status200, status422)
import Network.Wai
import qualified Post as PS
import qualified PostDTO as P
import Prelude hiding (read)
import qualified User as U
import RequestsUtils
  
getUsers :: Request -> Connection -> IO Response
getUsers request =
  let (page, sortBy, _) = getAdditionalParams request
      in fmap respondJson .
          (read page sortBy :: Connection -> IO [U.User])

createUser :: Request -> Connection -> IO Response
createUser request = actionWithBody request createUserBs

createUserBs :: B.ByteString -> Connection -> IO Response
createUserBs = createModel . decodeUser

deleteUser :: Request -> Connection -> IO Response
deleteUser request = authorizeRequest request $
  actionWithBody request deleteUserBs

deleteUserBs :: B.ByteString -> Connection -> IO Response
deleteUserBs = deleteModel . (eitherDecode :: B.ByteString -> Either String U.UserId)