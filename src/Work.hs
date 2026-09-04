module Work where

import Graphics.Scad
import Graphics.Scad.Gear

render :: IO ()
render = writeScad "work.scad" model7

-- | A four-planet herringbone gearbox on ISO full-depth teeth, at a 20°
-- pressure angle, with a planet count that divides @sunTeeth + ringTeeth@, so
-- the planets land on exact even spacing rather than being nudged onto a
-- compromise angle.
--
-- This is meant to come off a printer turning, so what binds is not the mesh
-- but the nozzle.  Two millimetre-scale widths have to survive it: the gap
-- 'backlash' opens between a planet and the gears either side of it, and the
-- land left on top of a planet's tooth once that gap has been cut out of it.
-- At a module this small the second is the tighter of the two, and it is what
-- picks the tooth counts.  Equal sun and planet teeth keep both profile shifts
-- near their floor; six-tooth planets do not, and the 0.8-module shift they
-- need leaves so little land that any offset worth having points the tooth.
--
-- 'minShift' is that floor -- shift less than it and the rack undercuts the
-- flank -- but sitting on it is wrong here, and counterintuitively so.
-- 'planetary' cuts a shifted tip back by 'tipShortening', which grows with the
-- shift, and a tooth is fatter the further down it is cut; so a shift well
-- above the floor buys land rather than spending it.  What it costs is contact
-- ratio, and the helix buys that back more cheaply.
--
-- The helix is the other half of that trade.  A transverse contact ratio below
-- one -- which is where an eight-tooth pair sits at any shift this design can
-- use -- means no tooth pair is continuously engaged in any one cross-section,
-- and only the overlap the helix contributes keeps the mesh alive.  That
-- overlap is linear in the face width, which is why this box is as tall as it
-- is wide rather than the flatter golden-ratio proportion it used to be: the
-- taller face lets the helix come back down to 30°, and a shallower helix
-- spends less land on 'transverseBacklash'.  A 45° one would spend a third of
-- it.
--
-- Eight-tooth planets put @sunTeeth + ringTeeth@ at 32, so four planets divide
-- it exactly and none of them is nudged off even spacing.
--
-- The teeth here are around a millimetre across, so OpenSCAD's default
-- tessellation is far too coarse for them: at @$fa = 12@ the root circles come
-- out as 30-gons, and at @$fs = 2@ 'herringbone' would put over 5° of twist
-- between one slice and the next -- more than a tooth spans at its tip.
--
-- 'herringbone' builds each gear about @z = 0@, the plane its two halves
-- mirror in, so a gearbox comes out straddling that plane with half of it
-- below.  This one is lifted by half its height to stand on the bed instead,
-- which is where a slicer wants it.
model7 :: Form'
model7 =
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

model7' :: Form'
model7' =
  linearExtrude 10 True 1 0 $
    fa 3 . fs 0.2 $
      gear
        (defaultInvolute 5)
        12
