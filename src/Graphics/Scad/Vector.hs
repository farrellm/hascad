-- | Minimal two- and three-element vectors.
--
-- The DSL only ever uses these as containers of coordinates, so rather than
-- depend on @linear@ we define them here, with the numeric instances that make
-- writing coordinates by hand pleasant.
module Graphics.Scad.Vector
  ( V2 (..),
    V3 (..),
  )
where

data V2 a = V2 a a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data V3 a = V3 a a a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative V2 where
  pure a = V2 a a
  V2 f g <*> V2 a b = V2 (f a) (g b)

instance Applicative V3 where
  pure a = V3 a a a
  V3 f g h <*> V3 a b c = V3 (f a) (g b) (h c)

instance (Num a) => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Num a) => Num (V3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
