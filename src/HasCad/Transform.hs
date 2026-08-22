-- | Transformations and the rendering modifiers.
module HasCad.Transform
  ( translate,
    rotate,
    rotate',
    rotate2d,
    scale,
    resize,
    mirror,
    linearExtrude,
    projection,
    project,
    cut,
    offsetR,
    offsetDelta,
    hidden,
    debug,
    background,
  )
where

import Effectful (Eff)
import Effectful.Reader.Static qualified as Reader
import HasCad.Dimension (V)
import HasCad.Model
import HasCad.Monad
import HasCad.Vector (V3 (..))

translate :: (HasScad es) => V d Double -> Eff es (Model d) -> Eff es (Model d)
translate v = fmap (Translate v)

-- | Rotate by an angle about an axis.
rotate :: (HasScad es) => Double -> V3 Double -> Eff es Form -> Eff es Form
rotate a v = fmap (RotateV (Radian a) v)

-- | Rotate about the x, y and z axes in turn.
rotate' :: (HasScad es) => V3 Double -> Eff es Form -> Eff es Form
rotate' a = fmap (RotateA (Radian <$> a))

rotate2d :: (HasScad es) => Double -> Eff es Shape -> Eff es Shape
rotate2d a = fmap (RotateA (Radian <$> V3 0 0 a))

scale :: (HasScad es) => V d Double -> Eff es (Model d) -> Eff es (Model d)
scale v = fmap (Scale v)

resize :: (HasScad es) => V d Double -> Eff es (Model d) -> Eff es (Model d)
resize v = fmap (Resize v)

mirror :: (HasScad es) => V d Double -> Eff es (Model d) -> Eff es (Model d)
mirror v = fmap (Mirror v)

-- | @linearExtrude height center convexity twist@, where @twist@ is in
-- radians.
linearExtrude ::
  (HasScad es) =>
  Double ->
  Bool ->
  Int ->
  Double ->
  Eff es Shape ->
  Eff es Form
linearExtrude h c v t m =
  LinearExtrude h c v (Radian t) <$> Reader.ask <*> m

projection :: (HasScad es) => Bool -> Eff es Form -> Eff es Shape
projection c = fmap (Projection c)

-- | Project a solid onto the xy-plane.
project :: (HasScad es) => Eff es Form -> Eff es Shape
project = projection False

-- | Take the cross-section of a solid at the xy-plane.
cut :: (HasScad es) => Eff es Form -> Eff es Shape
cut = projection True

offsetR :: (HasScad es) => Double -> Bool -> Eff es Shape -> Eff es Shape
offsetR r c = fmap (Offset (OffsetR r) c)

offsetDelta :: (HasScad es) => Double -> Bool -> Eff es Shape -> Eff es Shape
offsetDelta d c = fmap (Offset (OffsetDelta d) c)

-- | Ignore this subtree (OpenSCAD's @*@).
hidden :: (HasScad es) => Eff es (Model d) -> Eff es (Model d)
hidden = fmap Hidden

-- | Highlight this subtree (OpenSCAD's @#@).
debug :: (HasScad es) => Eff es (Model d) -> Eff es (Model d)
debug = fmap Debug

-- | Draw this subtree as background only (OpenSCAD's @%@).
background :: (HasScad es) => Eff es (Model d) -> Eff es (Model d)
background = fmap Background
