-- | The dimension a model lives in, and the singleton that lets code be
-- written once for both.
module Graphics.Scad.Dimension
  ( Dimension (..),
    V,
    SDim (..),
    KnownDim (..),
  )
where

import Graphics.Scad.Vector (V2, V3)

data Dimension = Two | Three
  deriving stock (Show, Eq, Ord)

-- | The vector type of a given dimension.
type family V (d :: Dimension) :: Type -> Type where
  V 'Two = V2
  V 'Three = V3

-- | Singleton for 'Dimension'.  Matching on it refines @d@, which is what
-- makes a single @V d@-consuming function possible.
data SDim d where
  STwo :: SDim 'Two
  SThree :: SDim 'Three

deriving stock instance Show (SDim d)

deriving stock instance Eq (SDim d)

class KnownDim d where
  sdim :: SDim d

instance KnownDim 'Two where
  sdim = STwo

instance KnownDim 'Three where
  sdim = SThree
