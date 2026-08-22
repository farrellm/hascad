{-# LANGUAGE NoFieldSelectors #-}

-- | Tessellation settings, corresponding to OpenSCAD's @$fa@, @$fs@, @$fn@
-- special variables (plus @linear_extrude@'s @slices@).
--
-- A 'Facet' is carried in a reader effect and captured by the primitives that
-- need it; see "Graphics.Scad.Monad" for the setters.  The fields are named
-- after those setters, which is what NoFieldSelectors is for: read them with
-- record dot syntax rather than a selector function.
module Graphics.Scad.Facet
  ( Facet (..),
    defaultFacet,
  )
where

data Facet = Facet
  { fa :: Maybe Double,
    fs :: Maybe Double,
    fn :: Maybe Double,
    slices :: Maybe Int
  }
  deriving stock (Show, Eq, Ord)

defaultFacet :: Facet
defaultFacet =
  Facet {fa = Nothing, fs = Nothing, fn = Nothing, slices = Nothing}
