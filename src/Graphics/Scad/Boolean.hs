-- | Constructive solid geometry: the boolean operators and their n-ary forms,
-- and signed 'Part's that carry cuts as well as material.
--
-- The operators flatten as they go, so a chain of unions renders as a single
-- @union()@ rather than a tower of nested ones.
module Graphics.Scad.Boolean
  ( SetLike (..),
    Union (..),
    Intersection (..),
    Signed (..),
    Part (..),
    positive,
    negative,
    realize,
    union,
    intersection,
    difference,
    hull,
    minkowski,
  )
where

import Effectful (Eff)
import Graphics.Scad.Model
import Graphics.Scad.Monad (HasScad)

infixl 6 <||>

infixl 6 <&&>

infixl 6 <->

class SetLike a where
  -- | Union.
  (<||>) :: a -> a -> a

  -- | Intersection.
  (<&&>) :: a -> a -> a

  -- | Difference.
  (<->) :: a -> a -> a

instance SetLike (Model d) where
  Union' xs <||> Union' ys = Union' (xs <> ys)
  x <||> Union' ys = Union' (x : ys)
  Union' xs <||> y = Union' (xs <> [y])
  x <||> y = Union' [x, y]

  Intersection' xs <&&> Intersection' ys = Intersection' (xs <> ys)
  x <&&> Intersection' ys = Intersection' (x : ys)
  Intersection' xs <&&> y = Intersection' (xs <> [y])
  x <&&> y = Intersection' [x, y]

  Difference x y <-> a = Difference x (y <||> a)
  x <-> y = Difference x y

instance (SetLike a, Applicative m) => SetLike (m a) where
  (<||>) = liftA2 (<||>)
  (<&&>) = liftA2 (<&&>)
  (<->) = liftA2 (<->)

newtype Union a = Union {getUnion :: a}

instance (SetLike a) => Semigroup (Union a) where
  Union a <> Union b = Union (a <||> b)

instance Monoid (Union (Model d)) where
  mempty = Union (Union' [])

instance (Applicative m) => Monoid (Union (m (Model d))) where
  mempty = Union (pure (Union' []))

newtype Intersection a = Intersection {getIntersection :: a}

instance (SetLike a) => Semigroup (Intersection a) where
  Intersection a <> Intersection b = Intersection (a <&&> b)

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

-- | A model contributed with a sign: as material, or as a cut.
data Signed a = Positive a | Negative a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- | Material and cuts, applied left to right: each cut removes everything to
-- its left, but nothing that comes after it.  This lets a component carry its
-- own clearance, such as a boss together with the hole through what it is
-- mounted on:
--
-- > boss = positive (cylinder 4 6) <> negative (cylinder 10 2)
-- > bracket = realize (positive plate <> boss)
--
-- 'Part' is the free monoid on signed models, so the monoid laws hold
-- structurally.  'realize' interprets it so that
--
-- > realize (positive x <> positive y) == x <||> y
-- > realize (positive x <> negative y) == x <-> y
--
-- Read as the point-set function @S ↦ (S − N) ∪ P@, parts compose as a
-- pointwise "last write wins", so the interpretation respects the laws too.
-- 'fmap' transforms material and cuts alike.
newtype Part a = Part [Signed a]
  deriving stock (Show, Eq, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

positive :: a -> Part a
positive x = Part [Positive x]

negative :: a -> Part a
negative x = Part [Negative x]

-- | Collapse a 'Part' to the model it describes.  Cuts with nothing to their
-- left are dropped; a part with no material is the empty union.
realize :: (SetLike a, Monoid (Union a)) => Part a -> a
realize (Part xs) = fromMaybe (getUnion mempty) (foldl' step Nothing xs)
  where
    step acc (Positive x) = Just (maybe x (<||> x) acc)
    step acc (Negative x) = (<-> x) <$> acc
