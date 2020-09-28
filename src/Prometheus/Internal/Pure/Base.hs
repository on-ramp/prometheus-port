{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Prometheus.Internal.Pure.Base where

import Data.Default
import qualified Data.Map as Map
import Protolude

--  Data Structures

-- A type family that removes Identity from underlying data structures.
type family NoIdentity f a where
  NoIdentity Identity a = a
  NoIdentity f a = f a

-- Every metric is defined as a newtype over the value, as:
--
-- - Different metrics may have access to different typeclasses,
-- but the same inner representation (e.g. Counter and Gauge)
--
-- - Type families ('NoIdentity' being one) are a pain in the butt to work with,
-- so they need a wrapper.
newtype Pure f s = Pure
  { unPure :: NoIdentity f s
  }

instance Default (NoIdentity f s) => Default (Pure f s) where
  def = Pure def

--  Operations over metric types

-- * Name retrieval

-- Every metric needs a name, so it may as well have a pure version
class PureNamed a where
  pureName :: Proxy a -> LByteString

instance PureNamed a => PureNamed (Pure Identity a) where
  pureName (Proxy :: Proxy (Pure Identity a)) = pureName (Proxy :: Proxy a)

-- * Construction

class PureConstructible c a where
  pureConstruct :: c -> a

instance PureConstructible c a => PureConstructible c (Pure Identity a) where
  pureConstruct = Pure . pureConstruct

-- * Extracting values

-- A type family that bridges extraction of data from metrics. For example, saying
--
-- > instance PureExtractable Double Counter where
-- >   pureExtract = unCounter
--
-- is absolutely valid, but would send the compiler into a stupor if you use GHCi,
-- because you definitely want to find () in there, right?
type family Extract (a :: *)

class
  Extract a ~ e =>
  PureExtractable e a
  where
  pureExtract :: a -> e

type instance Extract (Pure Identity a) = Extract a

instance PureExtractable e a => PureExtractable e (Pure Identity a) where
  pureExtract = pureExtract . unPure

-- * Exporting

data PureExportSample
  = ExportSample [Sample]
  | NoSample

instance Monoid PureExportSample where
  mempty = NoSample

instance Semigroup PureExportSample where
  (<>) x NoSample = x
  (<>) NoSample x = x
  (<>) (ExportSample x) (ExportSample y) = ExportSample (x <> y)

-- Sample represents a single observation line
data Sample
  = DoubleSample LByteString Tags Double
  | IntSample LByteString Tags Int
  deriving (Show)

addTags :: Tags -> PureExportSample -> PureExportSample
addTags _ NoSample = NoSample
addTags tags (ExportSample xs) = ExportSample (addTags' tags <$> xs)

-- Vectors will later add their tags to 'Sample's with this function
addTags' :: Tags -> Sample -> Sample
addTags' tags (DoubleSample a t d) = DoubleSample a (tags <> t) d
addTags' tags (IntSample a t d) = IntSample a (tags <> t) d

class PureExportable a where
  pureExport :: a -> PureExportSample

instance PureExportable a => PureExportable (Pure Identity a) where
  pureExport = pureExport . unPure

-- * Collecting data

class PureIncrementable a where
  pureIncrement :: a -> a
  (+.+) :: a -> Double -> a

instance PureIncrementable s => PureIncrementable (Pure Identity s) where
  pureIncrement = Pure . pureIncrement . unPure
  (+.+) a d = Pure . (+.+ d) $ unPure a

class PureDecrementable a where
  pureDecrement :: a -> a
  (-.-) :: a -> Double -> a

instance PureDecrementable s => PureDecrementable (Pure Identity s) where
  pureDecrement = Pure . pureDecrement . unPure
  (-.-) a d = Pure . (-.- d) $ unPure a

class PureSettable a where
  (=.=) :: a -> Double -> a

instance PureSettable s => PureSettable (Pure Identity s) where
  (=.=) a d = Pure . (=.= d) $ unPure a

class PureObservable a where
  pureObserve :: a -> Double -> a

instance PureObservable s => PureObservable (Pure Identity s) where
  pureObserve m a = Pure . (`pureObserve` a) $ unPure m

--  Vector precursors

type Tags = [(LByteString, LByteString)]

-- A label is an n-tuple of 'LByteString's used to tag metrics
class
  Ord l =>
  Label l
  where
  -- Compresses label and value into tuples of 'LByteString's

  flatten :: l -> l -> Tags

instance Label LByteString where
  flatten a b = [(a, b)]

instance Label (LByteString, LByteString) where
  flatten (a, b) (c, d) = [(a, c), (b, d)]

instance Label (LByteString, LByteString, LByteString) where
  flatten (a, b, c) (d, e, f) = [(a, d), (b, e), (c, f)]

instance Label (LByteString, LByteString, LByteString, LByteString) where
  flatten (a, b, c, d) (e, f, g, h) = [(a, e), (b, f), (c, g), (d, h)]

-- Class that allows transformation from @(Pure Identity s)@ to @(Pure (Map l) s)@
class
  Label l =>
  PureMappable l a b
    | a -> b
  where
  pmap :: l -> b -> (b -> b) -> a -> a

instance (Label l) => PureMappable l (Pure (Map l) b) b where
  pmap l d f = Pure . Map.alter (Just . f . fromMaybe d) l . unPure
