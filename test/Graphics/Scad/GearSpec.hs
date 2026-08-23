module Graphics.Scad.GearSpec (spec) where

import Data.List (maximum, minimum)
import Data.Text qualified as T
import Graphics.Scad
import Graphics.Scad.Facet (Facet (..))
import Graphics.Scad.Gear
import Test.Hspec

shape :: Shape' -> Text
shape = renderText

-- | The pressure angle the example gearboxes are drawn with, which is steep
-- enough to put the base circle well inside the root.
steep :: Double -> Involute
steep m = (defaultInvolute m) {pressureAngle = pi * 45 / 180}

radius :: V2 Double -> Double
radius (V2 x y) = sqrt (x * x + y * y)

spec :: Spec
spec = do
  describe "derived radii" $ do
    it "pitch radius is half the pitch diameter" $
      pitchRadius (defaultInvolute 2.5) 9 `shouldBe` 11.25
    it "root and tip straddle the pitch circle" $ do
      let i = defaultInvolute 2.5
      rootRadius i 9 `shouldBe` 11.25 - 1.25 * 2.5
      tipRadius i 9 `shouldBe` 11.25 + 2.5
    it "an internal cutter swaps addendum and dedendum" $ do
      let i = internal (defaultInvolute 2.5)
      (i.addendum, i.dedendum) `shouldBe` (1.25 * 2.5, 2.5)

  describe "flank" $ do
    -- The bug this guards: at a conventional pressure angle the base circle
    -- lies outside the root, so a flank that stops at the base circle leaves
    -- the teeth floating free of the disc they are unioned onto.
    it "reaches the root when the base circle lies outside it" $ do
      let i = defaultInvolute 2.5
      baseRadius i 9 `shouldSatisfy` (> rootRadius i 9)
      minimum (fmap radius (flank i 9)) `shouldSatisfy` (<= rootRadius i 9)
    it "stops at the base circle when the root lies outside it" $ do
      let i = steep 2.5
      baseRadius i 9 `shouldSatisfy` (< rootRadius i 9)
      minimum (fmap radius (flank i 9)) `shouldBe` baseRadius i 9
    it "runs out to the tip" $ do
      let i = defaultInvolute 2.5
      maximum (fmap radius (flank i 9)) `shouldSatisfy` \r ->
        abs (r - tipRadius i 9) < 1e-9

  describe "gear" $ do
    it "emits one tooth module and applies it once per tooth" $ do
      let out = shape (gear (defaultInvolute 2.5) 9)
      -- Two modules: the tooth, and the gear that arranges copies of it.
      T.count "module mdl_" out `shouldBe` 2
      T.count "children();" out `shouldBe` 9 + 2
    it "closes the circle exactly" $ do
      let n = 9 :: Int
          theta = tau / fromIntegral n
      theta * fromIntegral n `shouldBe` (tau :: Double)

  describe "planetary" $
    it "derives the ring tooth count from the meshing constraint" $ do
      let p =
            Planetary
              { rOuter = 35,
                sunTeeth = 9,
                planetTeeth = 7,
                nPlanet = 5,
                backlash = 0.4,
                height = 20
              }
          i = steep 2.5
      ringTeeth p `shouldBe` 23
      pitchRadius i (ringTeeth p)
        `shouldBe` pitchRadius i p.sunTeeth + 2 * pitchRadius i p.planetTeeth

  -- The bug these guard: 'herringbone' draws a 45 degree helix, so a gear as
  -- small as the Work gearbox's planets twists 50.93° over each half.  Against
  -- the slices = 10 that model asked for, that is 5.09° from one slice to the
  -- next -- more than the 3.91° a tooth spans at its tip, or the 2.11° left
  -- after the backlash offset.  'linear_extrude' joins corresponding vertices
  -- between slices, so nothing comes apart, but consecutive cross-sections do
  -- not overlap and the teeth render as a stack of torn scales.
  describe "tessellation" $ do
    it "resolves a full circle the way OpenSCAD does" $
      fragments defaultFacet tau 9.6875 `shouldBe` 30
    it "tessellates a twist as the arc its cross-section sweeps" $ do
      let twist = 0.5 * 20 / 11.25
          fine = defaultFacet {fs = Just 0.2}
      -- 5.09° a slice at OpenSCAD's default $fs; 1.02° once the model asks for
      -- a resolution its millimetre-wide teeth can actually be drawn at.
      extrudeSlices defaultFacet twist 11.25 `shouldBe` 5
      extrudeSlices fine twist 11.25 `shouldBe` 50
    it "slices a slow twist as finely as a fast one" $ do
      -- Both halves of a 45° herringbone sweep half its height of arc,
      -- whatever the radius: the ring must not come out coarser than the
      -- planets just because it turns through a fifth of the angle.
      let f = defaultFacet {fs = Just 0.2}
      extrudeSlices f (0.5 * 20 / 33.75) 33.75 `shouldBe` 50
      extrudeSlices f (0.5 * 20 / 11.25) 11.25 `shouldBe` 50
    it "yields to an explicit slice count" $
      extrudeSlices defaultFacet {slices = Just 7} 1.0 11.25 `shouldBe` 7
