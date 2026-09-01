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
-- A planet this small is squeezed from both sides.  'minShift' is the floor --
-- shift less than that and the rack undercuts the flank -- and a pointed tooth
-- is the ceiling, which 'tipThickness' reports as it runs down to zero.  A
-- steeper pressure angle lowers the floor, but it also converges the flanks
-- sooner, and at these tooth counts that second effect wins: at 25° a
-- full-depth tooth is past its point on a six-tooth planet at every shift the
-- floor allows, and only a stub tooth brings it back.  At 20° the band is open
-- without one: the floor is 0.649 modules on the planets and 0.415 on the sun,
-- and the shifts below clear both with room to spare and still leave 1.03mm of
-- land at the planet tip.
--
-- The shifts are large, and they push the gears apart: the mesh runs at 33.4°
-- rather than 20°, and the planets orbit at 28.8 rather than the reference
-- 24.  The ring grows with them -- 'ringShift' is 2.15 -- and 'planetary'
-- then cuts its root back over the planets' shortened tips, leaving it at
-- 43.86, which is what 'rOuter' has to clear.
--
-- Six-tooth planets are what pin the planet count at four: they put
-- @sunTeeth + ringTeeth@ at 32, and of the counts that will fit around the sun
-- only four divides it.  Five would leave each planet nudged up to 5.6° off
-- even.
--
-- The teeth here are around a millimetre across, so OpenSCAD's default
-- tessellation is far too coarse for them: at @$fa = 12@ the root circles come
-- out as 30-gons, and at @$fs = 2@ 'herringbone' would put over 5° of twist
-- between one slice and the next -- more than a tooth spans at its tip.
model7 :: Form'
model7 =
  let phi = (1 + sqrt 5) / 2
      m = 3.2
      n = 6 :: Int
      h = fromIntegral n * m / phi
   in fa 3 . fs 0.2 $
        planetary
          (defaultInvolute m) {pressureAngle = pi * 20 / 180}
          Planetary
            { rOuter = 50,
              sunTeeth = 10,
              planetTeeth = n,
              sunShift = 0.55,
              planetShift = 0.80,
              nPlanet = 4,
              backlash = 0.2,
              height = h
            }

model7' :: Form'
model7' =
  linearExtrude 10 True 1 0 $
    fa 3 . fs 0.2 $
      gear
        (defaultInvolute 5)
        12
