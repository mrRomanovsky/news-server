module TagRequests where

import qualified Tag as C
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
import qualified Tag as T
import RequestsUtils
  
getTags :: Request -> Connection -> IO Response
getTags request =
  let (page, sortBy, _) = getAdditionalParams request
      in fmap respondJson .
          (read page sortBy :: Connection -> IO [T.Tag])

createTag :: Request -> Connection -> IO Response
createTag request = authorizeRequest request $
  actionWithBody request createTagBs

createTagBs :: B.ByteString -> Connection -> IO Response
createTagBs = createModel . decodeTag

deleteTag :: Request -> Connection -> IO Response
deleteTag request = authorizeRequest request $
  actionWithBody request deleteTagBs

deleteTagBs :: B.ByteString -> Connection -> IO Response
deleteTagBs = deleteModel . (eitherDecode :: B.ByteString -> Either String T.TagId)

updateTag :: Request -> Connection -> IO Response
updateTag request = authorizeRequest request $
  actionWithBody request updateTagBs

updateTagBs :: B.ByteString -> Connection -> IO Response
updateTagBs = updateModel . decodeTag