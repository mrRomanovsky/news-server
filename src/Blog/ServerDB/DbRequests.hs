{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Blog.ServerDB.DbRequests where

import Blog.Config.Config
import Blog.Models.Model
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types hiding (Query)
import Network.HTTP.Types (Query, hAuthorization, status200, status404)
import Network.Wai
import System.Environment

getConnection :: ServerConfig -> IO Connection
getConnection conf =
  connect
    defaultConnectInfo
      { connectDatabase = dbName conf
      , connectUser = dbUser conf
      , connectPassword = dbPassword conf
      }

getRecords ::
     FromRow m
  => T.Text
  -> Maybe Integer
  -> Maybe B.ByteString
  -> Connection
  -> IO [m]
getRecords table page sortParam conn =
  let defSort = getDefaultOrder table
      sortP = Identifier $ maybe defSort decodeUtf8 sortParam
   in query conn (paginate selectOrdered page) (Identifier table, sortP)

selectOrdered :: Database.PostgreSQL.Simple.Query
selectOrdered = "SELECT * FROM ? ORDER BY ?"

getDefaultOrder :: T.Text -> T.Text
getDefaultOrder table
  | "ies" `T.isSuffixOf` table =
    T.init (T.init $ T.init table) `T.append` "y_id"
  | otherwise = T.init table `T.append` "_id"

paginate ::
     Database.PostgreSQL.Simple.Query
  -> Maybe Page
  -> Database.PostgreSQL.Simple.Query
paginate q p =
  let offset = maybe 0 ((* 20) . (subtract 1)) p
      qStr = init $ tail $ show q
   in fromString $ qStr ++ "OFFSET " ++ show offset ++ " LIMIT 20"

paginatedQuery ::
     (FromRow m, ToRow q)
  => Database.PostgreSQL.Simple.Query
  -> q
  -> Maybe Page
  -> Connection
  -> IO [m]
paginatedQuery queryToPaginate params page conn =
  query conn (paginate queryToPaginate page) params
