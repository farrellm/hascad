module Work where

import Graphics.Scad
import Graphics.Scad.Gear

render :: IO ()
render = writeScad "work.scad" model7

-- | The gearbox as it stood before the radius was squeezed: eight-tooth sun
-- and planets on a 1.6 module, a 30° helix, and 26.5mm of ring blank.  Kept to
-- render beside 'model7', since every number in that one is a step down from a
-- number in this one.
model7' :: Form'
model7' =
  let m = 1.6
      n = 8 :: Int
      h = fromIntegral n * m
   in fa 3 . fs 0.2 . translate (V3 0 0 (h / 2)) $
        planetary
          (defaultInvolute m) {pressureAngle = pi * 20 / 180}
          Planetary
            { rOuter = 26.5,
              sunTeeth = n,
              planetTeeth = n,
              sunShift = 0.74,
              planetShift = 0.74,
              nPlanet = 4,
              backlash = 0.3,
              height = h,
              helixAngle = Just (pi * 30 / 180)
            }

-- | A four-planet herringbone gearbox on ISO full-depth teeth, at a 20°
-- pressure angle, drawn as small around as it will go.  Seven-tooth sun and
-- planets put @sunTeeth + ringTeeth@ at 28, so four planets divide it exactly
-- and none of them is nudged off even spacing; equal counts also make the
-- ratio exactly 4:1.
--
-- This is meant to come off a printer turning, so what binds is not the mesh
-- but the nozzle.  Two millimetre-scale widths have to survive it: the gap
-- 'backlash' opens between a planet and the gears either side of it, and the
-- land left on top of a planet's tooth once that gap has been cut out of it.
-- The land is what sets the size of the whole gearbox, because it scales with
-- the module while 'backlash' does not -- 0.3mm is what the printer wants
-- whatever the teeth are -- so shrinking the module walks the land down onto
-- the 0.4mm one-extrusion floor and stops there.  A 1.05 module is where it
-- stops: the planet's land comes out at 0.45mm.
--
-- 'rOuter' is then the ring's root circle -- 13.69, where the cutter's tip
-- reaches -- plus a rim.  1.2mm of rim is three perimeters, which is as thin as
-- a wall can be and still print solid where the ring's tooth roots scallop it.
--
-- Where the profile shift is spent matters more than how much of it there is,
-- and this is the one number here that is not obvious.  'ringShift' is
-- @sunShift + 2 planetShift@, so a module of shift on a planet grows the ring
-- twice as fast as a module of it on the sun; but the two enter
-- 'tipShortening' equally, and that shortening is what fattens the planet's
-- tip, since a tooth is wider the further down it is cut.  So sun shift buys
-- land almost free and planet shift pays double for less of it.  Hence 0.85 on
-- the sun against 0.65 on the planets, where the old gearbox carried the same
-- shift on both: sliding those 0.2 modules from the planets to the sun is
-- worth 0.15mm of land at no cost in radius.  The planets sit just clear of
-- 'minShift', which is 0.59 at seven teeth, and what stops the sun climbing
-- further is transverse contact ratio, which a shift spends.
--
-- The helix is where the rest of the contact ratio comes from, and a 12mm face
-- is what fixes it.  A transverse contact ratio below one -- 0.80 here, and
-- below one at every shift this design can use -- means no tooth pair is
-- continuously engaged in any one cross-section, and only the helix's overlap
-- keeps the mesh alive.  That overlap is @height tan beta / (pi m)@, so with
-- the height pinned the angle cannot flatten past about 17.5° without the
-- overlap falling under one, and it wants to be no steeper either: a steeper
-- helix spends more land on 'transverseBacklash'.  17.5° puts the overlap at
-- 1.15 and the total at 1.95.
--
-- The teeth here are around two thirds of a millimetre across, so OpenSCAD's
-- default tessellation is far too coarse for them: at @$fa = 12@ the root
-- circles come out as 30-gons, and at @$fs = 2@ 'herringbone' would cut each
-- half into the two slices it floors at, putting 15° of twist between one and
-- the next -- three times what a tooth spans at its tip.  @$fs = 0.15@, finer
-- than the 0.2 the larger gearbox wanted, brings that to 13 slices and 2.3°.
--
-- 'herringbone' builds each gear about @z = 0@, the plane its two halves
-- mirror in, so a gearbox comes out straddling that plane with half of it
-- below.  This one is lifted by half its height to stand on the bed instead,
-- which is where a slicer wants it.
model7 :: Form'
model7 =
  let m = 1.05
      n = 7 :: Int
      h = 12
   in fa 3 . fs 0.15 . translate (V3 0 0 (h / 2)) $
        planetary
          (defaultInvolute m) {pressureAngle = pi * 20 / 180}
          Planetary
            { rOuter = 14.9,
              sunTeeth = n,
              planetTeeth = n,
              sunShift = 0.85,
              planetShift = 0.65,
              nPlanet = 4,
              backlash = 0.3,
              height = h,
              helixAngle = Just (pi * 17.5 / 180)
            }
