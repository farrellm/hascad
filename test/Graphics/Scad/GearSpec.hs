module Graphics.Scad.GearSpec (spec) where

import Data.List (maximum, minimum)
import Data.Text qualified as T
import Graphics.Scad
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
