module Data.List.HList where

import           Protolude

data HList :: [(* -> *) -> *] -> * where
  HNil :: HList '[]
  (:#) :: x Identity -> HList xs -> HList (x ': xs)

infixr 5 :#

class Monad m => Apply f a b m where
  apply :: f -> a -> b -> m b

class Monad m => FoldrH f acc xs m where
  foldrH :: f -> acc -> HList xs -> m acc

