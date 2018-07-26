module Category where

import Model
import qualified Data.ByteString as B

data Category = Category {name :: B.ByteString, nested :: [Category]}

instance Model Category where
  create _ = return ()
  read = return []
  update _ _ = return ()
  delete _ = return ()
{-
Категории (могут быть вложенными, то есть одна категория является подкатегорией для другой) 
Допустим, есть категория "Языки программирования", и у нее может быть подкатегория "Динамически Типизированные ЯП", и далее, соответственно, подкатегория подкатегории "Python"  — и таких уровней вложенности может быть произвольное количество
-}