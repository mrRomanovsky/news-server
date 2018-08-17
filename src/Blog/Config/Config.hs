module Blog.Config.Config
  ( ServerConfig(..)
  , getConfig
  ) where

import Blog.Config.Logging
import Data.Maybe
import System.Environment

data ServerConfig = ServerConfig
  { dbName :: String
  , dbUser :: String
  , dbPassword :: String
  , logLevel :: Logging
  , logFile :: String
  , port :: Int
  }

getConfig :: IO ServerConfig
getConfig = do
  dbName <-
    fromMaybe (error "database name not specified!") <$> lookupEnv "DB_NAME"
  dbUser <- fromMaybe (error "user name not specified!") <$> lookupEnv "DB_USER"
  dbPassword <-
    fromMaybe (error "database password not specified!") <$>
    lookupEnv "DB_PASSW"
  logLevelStr <- fromMaybe "Warning" <$> lookupEnv "LOG_LEVEL"
  let logLevel =
        fromMaybe (error "incorrect log level!") $ getLogLevel logLevelStr
  logFile <- fromMaybe "news-server.log" <$> lookupEnv "LOG_FILE"
  portStr <- fromMaybe "3000" <$> lookupEnv "APP_PORT"
  let port = parsePort portStr
  return $ ServerConfig dbName dbUser dbPassword logLevel logFile port

parsePort :: String -> Int
parsePort s =
  case reads s of
    [(port, _)] -> port
    _ -> error "port should be an integer number!"
