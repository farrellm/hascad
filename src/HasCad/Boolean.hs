-- | Constructive solid geometry: the boolean operators and their n-ary forms.
--
-- The operators flatten as they go, so a chain of unions renders as a single
-- @union()@ rather than a tower of nested ones.
module HasCad.Boolean
  ( SetLike (..),
    Union (..),
    Intersection (..),
    union,
    intersection,
    difference,
    hull,
    minkowski,
  )
where

import Effectful (Eff)
import HasCad.Model
import HasCad.Monad (HasScad)

infixl 6 <+>

infixl 6 <#>

infixl 6 <->

class SetLike a where
  -- | Union.
  (<+>) :: a -> a -> a

  -- | Intersection.
  (<#>) :: a -> a -> a

  -- | Difference.
  (<->) :: a -> a -> a

instance SetLike (Model d) where
  Union' xs <+> Union' ys = Union' (xs <> ys)
  x <+> Union' ys = Union' (x : ys)
  Union' xs <+> y = Union' (xs <> [y])
  x <+> y = Union' [x, y]

  Intersection' xs <#> Intersection' ys = Intersection' (xs <> ys)
  x <#> Intersection' ys = Intersection' (x : ys)
  Intersection' xs <#> y = Intersection' (xs <> [y])
  x <#> y = Intersection' [x, y]

  Difference x y <-> a = Difference x (y <+> a)
  x <-> y = Difference x y

instance (SetLike a, Applicative m) => SetLike (m a) where
  (<+>) = liftA2 (<+>)
  (<#>) = liftA2 (<#>)
  (<->) = liftA2 (<->)

newtype Union a = Union {getUnion :: a}

instance (SetLike a) => Semigroup (Union a) where
  Union a <> Union b = Union (a <+> b)

instance Monoid (Union (Model d)) where
  mempty = Union (Union' [])

instance (Applicative m) => Monoid (Union (m (Model d))) where
  mempty = Union (pure (Union' []))

newtype Intersection a = Intersection {getIntersection :: a}

instance (SetLike a) => Semigroup (Intersection a) where
  Intersection a <> Intersection b = Intersection (a <#> b)

instance Monoid (Intersection (Model d)) where
  mempty = Intersection (Intersection' [])

instance (Applicative m) => Monoid (Intersection (m (Model d))) where
  mempty = Intersection (pure (Intersection' []))

union :: (HasScad es) => [Eff es (Model d)] -> Eff es (Model d)
union = getUnion . mconcat . fmap Union

intersection :: (HasScad es) => [Eff es (Model d)] -> Eff es (Model d)
intersection = getIntersection . mconcat . fmap Intersection

-- | Subtract every model in the list from the first.
difference ::
  (HasScad es) => Eff es (Model d) -> [Eff es (Model d)] -> Eff es (Model d)
difference x ys = liftA2 Difference x (union ys)

hull :: (HasScad es) => [Eff es (Model d)] -> Eff es (Model d)
hull ms = Hull <$> sequenceA ms

minkowski :: (HasScad es) => [Eff es (Model d)] -> Eff es (Model d)
minkowski ms = Minkowski <$> sequenceA ms
