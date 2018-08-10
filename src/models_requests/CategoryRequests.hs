module CategoryRequests where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Model
import Network.Wai
import Prelude hiding (read)
import qualified Category as C
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