module Work where

import Graphics.Scad
import Graphics.Scad.Gear

render :: IO ()
render = writeScad "work.scad" model7

-- | A four-planet herringbone gearbox on ISO full-depth teeth: a 20° pressure
-- angle, tooth counts above the 17-tooth undercut limit, and a planet count
-- that divides @sunTeeth + ringTeeth@, so the planets land on exact even
-- spacing rather than being nudged onto a compromise angle.
--
-- The teeth here are around a millimetre across, so OpenSCAD's default
-- tessellation is far too coarse for them: at @$fa = 12@ the root circles come
-- out as 30-gons, and at @$fs = 2@ 'herringbone' would put over 5° of twist
-- between one slice and the next -- more than a tooth spans at its tip.
model7 :: Form'
model7 =
  fa 3 . fs 0.2 $
    planetary
      (defaultInvolute 1.25)
      Planetary
        { rOuter = 39,
          sunTeeth = 18,
          planetTeeth = 18,
          nPlanet = 4,
          backlash = 0.2,
          height = 20
        }
