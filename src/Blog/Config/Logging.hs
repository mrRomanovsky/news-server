module Blog.Config.Logging
  ( Logging(..)
  , getLogLevel
  ) where

data Logging
  = None
  | Requests
  | Debug
  deriving (Eq, Ord)

getLogLevel :: String -> Maybe Logging
getLogLevel "None" = Just None
getLogLevel "Requests" = Just Requests
getLogLevel "Debug" = Just Debug
