{-# LANGUAGE TypeFamilies #-}

module Type.No where



type family No m f a where
  No m m a = a
  No m f a = f a
