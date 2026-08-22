-- | Tessellation settings, corresponding to OpenSCAD's @$fa@, @$fs@, @$fn@
-- special variables (plus @linear_extrude@'s @slices@).
--
-- A 'Facet' is carried in a reader effect and captured by the primitives that
-- need it; see "Graphics.Scad.Monad" for the setters.
module Graphics.Scad.Facet
  ( Facet (..),
    defaultFacet,
  )
where

data Facet = Facet
  { _fa :: Maybe Double,
    _fs :: Maybe Double,
    _fn :: Maybe Double,
    _slices :: Maybe Int
  }
  deriving stock (Show, Eq, Ord)

defaultFacet :: Facet
defaultFacet =
  Facet {_fa = Nothing, _fs = Nothing, _fn = Nothing, _slices = Nothing}
