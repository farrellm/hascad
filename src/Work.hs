module Work where

import Graphics.Scad
import Graphics.Scad.Gear

render :: IO ()
render = writeScad "work.scad" model7

-- | A four-planet herringbone gearbox on ISO full-depth teeth: a 20° pressure
-- angle, tooth counts above the 17-tooth undercut limit, and a planet count
-- that divides @sunTeeth + ringTeeth@, so the planets land on exact even
-- spacing rather than being nudged onto a compromise angle.
model7 :: Form'
model7 =
  slices 10 $
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
