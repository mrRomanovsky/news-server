module User where

import qualified Data.ByteString.Lazy as B
import Model

data User = User {name :: B.ByteString,
                  surname :: B.ByteString,
                  avatar :: B.ByteString,
                  creation_time :: B.ByteString,
                  isAdmin :: Bool}

instance Model User where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-
Пользователь
имя
фамилия
аватарка
дата создания
админ (булевая переменная)
-}