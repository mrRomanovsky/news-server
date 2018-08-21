module Blog.Config.Config
  ( ServerConfig(..)
  , getConfig
  ) where

import Blog.Config.Logging
import Data.Maybe
import System.Environment
import Text.Read (readMaybe)

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
  dbName <- getEnvErr "database name not specified!" "DB_NAME"
  dbUser <- getEnvErr "user name not specified!" "DB_USER"
  dbPassword <- getEnvErr "database password not specified!" "DB_PASSW"
  logLevelStr <- fromMaybe "Warning" <$> lookupEnv "LOG_LEVEL"
  let logLevel =
        fromMaybe (error "incorrect log level!") $ readMaybe logLevelStr
  logFile <- getEnvDef "news-server.log" "LOG_FILE"
  portStr <- getEnvDef "3000" "APP_PORT"
  let port = parsePort portStr
  return $ ServerConfig dbName dbUser dbPassword logLevel logFile port

getEnvErr :: String -> String -> IO String
getEnvErr errMes envVar = fromMaybe (error errMes) <$> lookupEnv envVar

getEnvDef :: String -> String -> IO String
getEnvDef defVal envVar = fromMaybe defVal <$> lookupEnv envVar

parsePort :: String -> Int
parsePort s =
  case reads s of
    [(port, _)] -> port
    _ -> error "port should be an integer number!"
