-- | The OpenSCAD primitive solids and shapes.
--
-- Primed variants are the un-centered ones; the unprimed defaults center the
-- primitive on the origin, which is almost always what you want.
module Graphics.Scad.Primitive
  ( circle,
    square,
    square',
    rectangle,
    rectangle',
    convex,
    polygon,
    sphere,
    cube,
    cube',
    box,
    box',
    cylinder,
    cylinder',
    cylinder2,
    cylinder2',
  )
where

import Effectful (Eff)
import Effectful.Reader.Static qualified as Reader
import Graphics.Scad.Model
import Graphics.Scad.Monad
import Graphics.Scad.Vector (V2, V3)

circle :: (HasScad es) => Double -> Eff es Shape
circle r = Circle r <$> Reader.ask

square :: (HasScad es) => Double -> Eff es Shape
square r = pure (Square r True)

square' :: (HasScad es) => Double -> Eff es Shape
square' r = pure (Square r False)

rectangle :: (HasScad es) => V2 Double -> Eff es Shape
rectangle r = pure (Rectangle r True)

rectangle' :: (HasScad es) => V2 Double -> Eff es Shape
rectangle' r = pure (Rectangle r False)

-- | A polygon with a single, convex boundary.
convex :: (HasScad es) => [V2 Double] -> Eff es Shape
convex vs = pure (Polygon vs [] Nothing)

polygon ::
  (HasScad es) => [V2 Double] -> [[Int]] -> Maybe Int -> Eff es Shape
polygon vs ps c = pure (Polygon vs ps c)

sphere :: (HasScad es) => Double -> Eff es Form
sphere r = Sphere r <$> Reader.ask

cube :: (HasScad es) => Double -> Eff es Form
cube r = pure (Cube r True)

cube' :: (HasScad es) => Double -> Eff es Form
cube' r = pure (Cube r False)

box :: (HasScad es) => V3 Double -> Eff es Form
box r = pure (Box r True)

box' :: (HasScad es) => V3 Double -> Eff es Form
box' r = pure (Box r False)

cylinder :: (HasScad es) => Double -> Double -> Eff es Form
cylinder h r = Cylinder h r True <$> Reader.ask

cylinder' :: (HasScad es) => Double -> Double -> Eff es Form
cylinder' h r = Cylinder h r False <$> Reader.ask

-- | A truncated cone of the given height and (bottom, top) radii.
cylinder2 :: (HasScad es) => Double -> (Double, Double) -> Eff es Form
cylinder2 h (r1, r2) = Cylinder2 h r1 r2 True <$> Reader.ask

cylinder2' :: (HasScad es) => Double -> (Double, Double) -> Eff es Form
cylinder2' h (r1, r2) = Cylinder2 h r1 r2 False <$> Reader.ask
