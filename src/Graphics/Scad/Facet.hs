{-# LANGUAGE NoFieldSelectors #-}

-- | Tessellation settings, corresponding to OpenSCAD's @$fa@, @$fs@, @$fn@
-- special variables (plus @linear_extrude@'s @slices@).
--
-- A 'Facet' is carried in a reader effect and captured by the primitives that
-- need it; see "Graphics.Scad.Monad" for the setters.  The fields are named
-- after those setters, which is what NoFieldSelectors is for: read them with
-- record dot syntax rather than a selector function.
--
-- OpenSCAD resolves @$fa@ and @$fs@ itself for the primitives it draws, so
-- most of the time these are merely passed through.  'fragments' reimplements
-- that rule for the one place hascad has to decide for itself — see
-- 'extrudeSlices'.
module Graphics.Scad.Facet
  ( Facet (..),
    defaultFacet,
    fragments,
    extrudeSlices,
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

-- | How many fragments an arc of @angle@ radians at radius @r@ is drawn with,
-- by OpenSCAD's own rule, generalised from the full circle it is written for:
-- a fragment spans at least @$fa@ of angle and at least @$fs@ of arc, so each
-- is a cap on the count and the coarser of the two wins.  @$fn@, when set,
-- fixes the count for a full turn and overrides both.
--
-- Unset fields take OpenSCAD's defaults, @$fa = 12@ and @$fs = 2@.  The result
-- is at least 1; callers impose their own floor.
fragments :: Facet -> Double -> Double -> Int
fragments f angle r =
  let angle' = abs angle
      r' = abs r
   in case f.fn of
        Just n | n > 0 -> max 1 (ceiling (n * angle' / (2 * pi)))
        _ ->
          let byAngle = angle' / (pi * fromMaybe 12 f.fa / 180)
              bySize = angle' * r' / fromMaybe 2 f.fs
           in max 1 (ceiling (min byAngle bySize))

-- | How many slices a twisted @linear_extrude@ of @twist@ radians, whose
-- cross-section is measured at radius @r@, should be cut into.
--
-- The cross-section sweeps @|twist| * r@ of arc as it climbs, and @$fs@ bounds
-- how much of that one slice may cover.  Only @$fs@: @$fa@ is the angle a
-- fragment of an /outline/ subtends, and tying the slice count to it couples
-- two unrelated resolutions -- on a gearbox, the ring twists a fifth as far as
-- the planets do, so an @$fa@ fine enough to slice the ring well buries the
-- model in circle fragments.  What a slice costs is how far the surface
-- travels, which is what @$fs@ measures.
--
-- An explicit 'slices' still wins: this is only the answer for a caller that
-- has not been told one.
extrudeSlices :: Facet -> Double -> Double -> Int
extrudeSlices f twist r =
  fromMaybe (max 2 (ceiling (abs twist * abs r / fromMaybe 2 f.fs))) f.slices
