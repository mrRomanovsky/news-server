module Blog.Config.Logging
  ( Logging(..)
  , getLogLevel
  ) where

data Logging
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

getLogLevel :: String -> Maybe Logging
getLogLevel "Debug" = Just Debug
getLogLevel "Info" = Just Info
getLogLevel "Warning" = Just Warning
getLogLevel "Error" = Just Error
