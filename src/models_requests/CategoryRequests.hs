module CategoryRequests where

import qualified Category as C
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

getCategories :: Request -> Connection -> IO Response
getCategories request =
  let (page, sortBy, _) = getAdditionalParams request
      in fmap respondJson .
          (read page sortBy :: Connection -> IO [C.Category])

createCategory :: Request -> Connection -> IO Response
createCategory request = authorizeRequest request $
  actionWithBody request createCategoryBs

createCategoryBs :: B.ByteString -> Connection -> IO Response
createCategoryBs = createModel . decodeCategory

deleteCategory :: Request -> Connection -> IO Response
deleteCategory request = authorizeRequest request $
  actionWithBody request deleteCategoryBs

deleteCategoryBs :: B.ByteString -> Connection -> IO Response
deleteCategoryBs = deleteModel . (eitherDecode :: B.ByteString -> Either String C.CategoryId)

updateCategory :: Request -> Connection -> IO Response
updateCategory request = authorizeRequest request $
  actionWithBody request updateCategoryBs

updateCategoryBs :: B.ByteString -> Connection -> IO Response
updateCategoryBs = updateModel . decodeCategory