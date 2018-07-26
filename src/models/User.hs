module User where

import qualified Data.ByteString.Lazy as B

data User = User {name :: B.ByteString,
                  surname :: B.ByteString,
                  avatar :: B.ByteString,
                  creation_time :: B.ByteString,
                  isAdmin :: Bool}
{-
Пользователь
имя
фамилия
аватарка
дата создания
админ (булевая переменная)
-}