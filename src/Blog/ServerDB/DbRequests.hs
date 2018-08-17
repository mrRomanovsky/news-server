{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Blog.ServerDB.DbRequests where

import Blog.Config.Config
import Blog.Models.Model
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
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

type WhereParam = (B.ByteString, B.ByteString)

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
getRecords table page sortParam = getRecordsWhere table page sortParam Nothing

getRecordsWhere ::
     FromRow m
  => T.Text
  -> Maybe Integer
  -> Maybe B.ByteString
  -> Maybe WhereParam
  -> Connection
  -> IO [m]
getRecordsWhere table page sortParam whereParam conn = do
  let defSort = getDefaultOrder table
      sortP = Identifier $ maybe defSort decodeUtf8 sortParam
      queryGet = paginate (orderBy $ addParam select whereParam) page
  print queryGet
  query conn queryGet (Identifier table, sortP)

select :: Database.PostgreSQL.Simple.Query
select = "SELECT * FROM ?"

orderBy :: Database.PostgreSQL.Simple.Query -> Database.PostgreSQL.Simple.Query
orderBy q =
  let qStr = getQueryStr q
   in fromString $ qStr ++ " ORDER BY ?"

getDefaultOrder :: T.Text -> T.Text
getDefaultOrder table
  | "ies" `T.isSuffixOf` table =
    T.init (T.init $ T.init table) `T.append` "y_id"
  | otherwise = T.init table `T.append` "_id"

addParam ::
     Database.PostgreSQL.Simple.Query
  -> Maybe WhereParam
  -> Database.PostgreSQL.Simple.Query
addParam q Nothing = q
addParam q (Just (par, val)) =
  let qStr = getQueryStr q
      p = unpack par
      v = unpack val
   in fromString $ qStr ++ " WHERE " ++ p ++ " = " ++ v

paginate ::
     Database.PostgreSQL.Simple.Query
  -> Maybe Page
  -> Database.PostgreSQL.Simple.Query
paginate q p =
  let offset = maybe 0 ((* 20) . (subtract 1)) p
      qStr = getQueryStr q
   in fromString $ qStr ++ " OFFSET " ++ show offset ++ " LIMIT 20"

paginatedQuery ::
     (FromRow m, ToRow q)
  => Database.PostgreSQL.Simple.Query
  -> q
  -> Maybe Page
  -> Connection
  -> IO [m]
paginatedQuery queryToPaginate params page conn =
  query conn (paginate queryToPaginate page) params

getQueryStr :: Database.PostgreSQL.Simple.Query -> String
getQueryStr = init . tail . show
