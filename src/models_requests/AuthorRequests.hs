module AuthorRequests
  ( getAuthors
  , createAuthor
  , deleteAuthor
  , updateAuthor
  ) where

import qualified Author as A
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import DbRequests
import Model
import Network.Wai
import Prelude hiding (read)
import RequestsUtils

getAuthors :: Request -> Connection -> IO Response
getAuthors request =
  let (page, sortBy, auth) = getAdditionalParams request
   in authResponse auth $
      fmap respondJson . (read page sortBy :: Connection -> IO [A.Author])

createAuthor :: Request -> Connection -> IO Response
createAuthor request =
  authorizeRequest request $ actionWithBody request createAuthorBs

createAuthorBs :: B.ByteString -> Connection -> IO Response
createAuthorBs = createModel . decodeAuthor

deleteAuthor :: Request -> Connection -> IO Response
deleteAuthor request =
  authorizeRequest request $ actionWithBody request deleteAuthorBs

deleteAuthorBs :: B.ByteString -> Connection -> IO Response
deleteAuthorBs =
  deleteModel . (eitherDecode :: B.ByteString -> Either String A.AuthorId)

updateAuthor :: Request -> Connection -> IO Response
updateAuthor request =
  authorizeRequest request $ actionWithBody request updateAuthorBs

updateAuthorBs :: B.ByteString -> Connection -> IO Response
updateAuthorBs = updateModel . decodeAuthor
