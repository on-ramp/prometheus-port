{-# LANGUAGE TypeFamilies #-}

module Type.No
  ( No
  ) where


-- | If @f ~ m@, then @a@.
-- Otherwise, @m a@.
type family No m f a where
  No m m a = a
  No m f a = f a
