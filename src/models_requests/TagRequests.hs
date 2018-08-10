module TagRequests
  ( getTags
  , createTag
  , updateTag
  , deleteTag
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Model
import Network.Wai
import Prelude hiding (read)
import RequestsUtils
import qualified Tag as T

getTags :: Request -> Connection -> IO Response
getTags request =
  let (page, sortBy, _) = getAdditionalParams request
   in fmap respondJson . (read page sortBy :: Connection -> IO [T.Tag])

createTag :: Request -> Connection -> IO Response
createTag request =
  authorizeRequest request $ actionWithBody request createTagBs

createTagBs :: B.ByteString -> Connection -> IO Response
createTagBs = createModel . decodeTag

deleteTag :: Request -> Connection -> IO Response
deleteTag request =
  authorizeRequest request $ actionWithBody request deleteTagBs

deleteTagBs :: B.ByteString -> Connection -> IO Response
deleteTagBs =
  deleteModel . (eitherDecode :: B.ByteString -> Either String T.TagId)

updateTag :: Request -> Connection -> IO Response
updateTag request =
  authorizeRequest request $ actionWithBody request updateTagBs

updateTagBs :: B.ByteString -> Connection -> IO Response
updateTagBs = updateModel . decodeTag
