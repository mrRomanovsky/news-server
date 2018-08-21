module Blog.Config.Logging
  ( Logging(..)
  ) where

data Logging
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show)
