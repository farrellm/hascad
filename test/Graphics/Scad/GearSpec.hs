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

-- | Equal to within the slack a couple of transcendental round trips leave.
shouldBeNear :: Double -> Double -> Expectation
shouldBeNear a b = abs (a - b) `shouldSatisfy` (< 1e-9)

infix 1 `shouldBeNear`

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
                sunShift = 0,
                planetShift = 0,
                nPlanet = 5,
                backlash = 0.4,
                height = 20
              }
          i = steep 2.5
      ringTeeth p `shouldBe` 23
      pitchRadius i (ringTeeth p)
        `shouldBe` pitchRadius i p.sunTeeth + 2 * pitchRadius i p.planetTeeth

  describe "profile shift" $ do
    let i = defaultInvolute 2.5
        x = 0.4
        i' = shifted x i
    it "moves the root and the tip out by the shift, and nothing else" $ do
      pitchRadius i' 9 `shouldBe` pitchRadius i 9
      baseRadius i' 9 `shouldBe` baseRadius i 9
      rootRadius i' 9 - rootRadius i 9 `shouldBeNear` x * 2.5
      tipRadius i' 9 - tipRadius i 9 `shouldBeNear` x * 2.5
    it "fattens the tooth at the pitch circle" $ do
      toothThickness i `shouldBeNear` 2.5 * pi / 2
      let fatter = toothThickness i' - toothThickness i
      fatter `shouldBeNear` 2 * x * 2.5 * tan i.pressureAngle
    it "leaves less land at the tip the further it goes" $ do
      tipThickness i 9 `shouldSatisfy` (> 0)
      tipThickness (shifted 1.2 i) 9 `shouldSatisfy` (< tipThickness i 9)

    -- A ring's tooth is only what its space leaves of the circular pitch, and
    -- that space is an external tooth, which fattens toward its own root.  So
    -- a ring tooth thins the further in it goes, and its tip -- the innermost
    -- radius, not the outermost -- is where it would point.
    it "measures a ring's tooth at its tip, which is its innermost radius" $ do
      -- Pull an unshifted ring's tip back onto its own pitch circle and the
      -- tooth there is half the circular pitch, like any other.
      let onPitch = (internal i) {dedendum = 0}
      ringTipThickness onPitch 23 `shouldBeNear` 2.5 * pi / 2
      -- A ring reaching further in has less tooth left at the tip than that.
      ringTipThickness (internal i) 23
        `shouldSatisfy` (< ringTipThickness onPitch 23)
    it "reports a pointed ring tooth as one" $ do
      -- Nothing sensible points, so this takes a ring shifted hard with its
      -- teeth driven much further in than a gearbox would ask for.
      let pointed = (internal (shifted 1.8 i)) {dedendum = 6.0}
      ringTipThickness pointed 23 `shouldSatisfy` (< 0)
    it "cannot point a ring tooth that ends inside the base circle" $ do
      -- There 'flank' draws a radial line, and radial lines do not converge.
      let deep = (internal i) {dedendum = 3 * 2.5}
      rootRadius deep 23 `shouldSatisfy` (< baseRadius deep 23)
      ringTipThickness deep 23 `shouldSatisfy` (> 0)
    -- The undercut limit a 20 degree rack is famous for: 2 / sin^2 20, which
    -- is 17.1, so seventeen teeth need a shift and eighteen do not.
    it "is needed below the undercut limit and not above it" $ do
      minShift i 17 `shouldSatisfy` (> 0)
      minShift i 18 `shouldSatisfy` (< 0)
    it "keeps a shifted internal cutter on the ring's radii" $ do
      -- The cutter cuts to where the ring's own root and tip are, and carries
      -- the ring's shift as it is -- see 'internal'.
      let cutter = internal (shifted x i)
      rootRadius cutter 23 `shouldBeNear` pitchRadius i 23 - i.addendum + x * 2.5
      tipRadius cutter 23 `shouldBeNear` pitchRadius i 23 + i.dedendum + x * 2.5
      cutter.shift `shouldBe` x

  describe "meshing" $ do
    let i = defaultInvolute 2.5
    it "inverts the involute function" $
      forM_ [10, 20, 30, 45 :: Double] $ \d ->
        let a = pi * d / 180 in invInvol (invol a) `shouldBeNear` a
    it "leaves the pressure angle alone when the shifts cancel" $
      workingAngle i 24 0 `shouldBe` i.pressureAngle
    it "steepens as the shifts add up" $
      workingAngle i 24 0.8 `shouldSatisfy` (> i.pressureAngle)

  describe "shifted planetary" $ do
    let i = defaultInvolute 3.2
        p =
          Planetary
            { rOuter = 44,
              sunTeeth = 10,
              planetTeeth = 5,
              sunShift = 0.45,
              planetShift = 0.72,
              nPlanet = 5,
              backlash = 0.2,
              height = 10
            }
        flat = p {sunShift = 0, planetShift = 0}
    it "derives the ring shift the way it derives the ring teeth" $
      ringShift p `shouldBe` p.sunShift + 2 * p.planetShift
    -- Both of a planet's meshes are the same physical gap, so the angle they
    -- run at had better come out the same.  That is what fixes 'ringShift'.
    it "runs both of a planet's meshes at one working angle" $
      workingAngle i (p.sunTeeth + p.planetTeeth) (p.sunShift + p.planetShift)
        `shouldBeNear` workingAngle
          i
          (ringTeeth p - p.planetTeeth)
          (ringShift p - p.planetShift)
    it "moves the planets out, and only when shifted" $ do
      centerDistance i flat
        `shouldBe` pitchRadius i flat.sunTeeth + pitchRadius i flat.planetTeeth
      centerDistance i p `shouldSatisfy` (> centerDistance i flat)
      tipShortening i flat `shouldBe` 0
      tipShortening i p `shouldSatisfy` (> 0)
    -- The bug this guards: 'internal' used to negate the shift on its way to
    -- the ring's cutter, on the reasoning that the cutter's teeth are the
    -- ring's spaces.  But the convention an internal mesh is reckoned in
    -- already accounts for that swap, so negating put the ring a whole tooth
    -- out: at the shifts below its spaces came out 0.02mm wide against a
    -- planet tooth of 6.9, and the ring rendered as a rim with its teeth
    -- floating free of it.
    --
    -- Thickness has to be compared on the circles the gears actually touch
    -- on, not on the reference circles -- at these shifts the ring's
    -- reference tooth is a sliver either way, and only the operating circle
    -- tells the two conventions apart.
    it "meshes without backlash on both of a planet's meshes" $ do
      let a' = centerDistance i p
          aw = workingAngle i (p.sunTeeth + p.planetTeeth) (p.sunShift + p.planetShift)
          -- A thickness carried from the reference circle out to the
          -- operating one.
          at t z r' = r' * (t / pitchRadius i z + 2 * (invol i.pressureAngle - invol aw))
          nExt = p.sunTeeth + p.planetTeeth
          nInt = ringTeeth p - p.planetTeeth
          rSun' = a' * fromIntegral p.sunTeeth / fromIntegral nExt
          rPlanet' = a' * fromIntegral p.planetTeeth / fromIntegral nExt
          -- The internal mesh rolls on its own, larger pair of circles.
          rPlanetI' = a' * fromIntegral p.planetTeeth / fromIntegral nInt
          rRing' = a' * fromIntegral (ringTeeth p) / fromIntegral nInt
          sSun = at (toothThickness (shifted p.sunShift i)) p.sunTeeth rSun'
          sPlanet = at (toothThickness (shifted p.planetShift i)) p.planetTeeth rPlanet'
          sPlanetI = at (toothThickness (shifted p.planetShift i)) p.planetTeeth rPlanetI'
          -- The ring's space is the cutter's tooth.
          eRing = at (toothThickness (internal (shifted (ringShift p) i))) (ringTeeth p) rRing'
      -- External: a tooth and the space it drops into fill one whole pitch.
      sSun + sPlanet `shouldBeNear` tau * rSun' / fromIntegral p.sunTeeth
      -- Internal: the ring's space is exactly the planet tooth that fills it.
      eRing `shouldBeNear` sPlanetI

    -- The bug this guards: the tips grow by the whole shift but the centres
    -- only part of it, so without 'tipShortening' the sun would bottom out in
    -- the planet's root and the gearbox would seize.
    it "keeps the root clearance a shortened tip is meant to keep" $ do
      let dTip = tipShortening i p * i.module'
          short j = j {addendum = j.addendum - dTip}
          sun = short (shifted p.sunShift i)
          planet = short (shifted p.planetShift i)
          clearance = i.dedendum - i.addendum
          -- What is left between one gear's tip and the other's root, on the
          -- line joining their axes.
          gap rTip rRoot = centerDistance i p - rTip - rRoot
      gap (tipRadius sun p.sunTeeth) (rootRadius planet p.planetTeeth)
        `shouldBeNear` clearance
      gap (tipRadius planet p.planetTeeth) (rootRadius sun p.sunTeeth)
        `shouldBeNear` clearance

    -- The bug this guards: 'tipShortening' was taken off the sun and the
    -- planets only, on the reasoning that an internal mesh gains the
    -- clearance an external one loses and so needs nothing done to it.  It
    -- does need something done to it -- the opposite thing.  Left alone, the
    -- ring stood k modules off the planets at one surface and 2k at the
    -- other, which at these shifts is 1.9mm and 3.0mm where 0.8 was wanted:
    -- the planets visibly rattled around inside a ring they barely reached.
    it "closes the ring onto the planets by as much as it opens the sun" $ do
      let dTip = tipShortening i p * i.module'
          planet = (shifted p.planetShift i) {addendum = i.addendum - dTip}
          c0 = internal (shifted (ringShift p) i)
          cutter = c0 {addendum = c0.addendum - 2 * dTip, dedendum = c0.dedendum + dTip}
          zr = ringTeeth p
          clearance = i.dedendum - i.addendum
          -- The planet sits a centre distance out from the ring's axis, so
          -- the ring's clearance over it is measured across that.
          over r rPlanet = r - centerDistance i p - rPlanet
      -- The ring's teeth reach down over the planet's root ...
      over (rootRadius cutter zr) (rootRadius planet p.planetTeeth)
        `shouldBeNear` clearance
      -- ... and its own root clears the planet's tip, by the same margin.
      over (tipRadius cutter zr) (tipRadius planet p.planetTeeth)
        `shouldBeNear` clearance
      -- Cutting the ring's root back also un-points the cutter, so the tooth
      -- spaces are swept to full depth ...
      tipThickness cutter zr `shouldSatisfy` (> 0)
      -- ... and reaching the ring's own teeth further in must not point those.
      -- At this shift the ring's tooth is a sliver where its reference circle
      -- falls, and only fattens out to a usable 2.5mm by the time it reaches
      -- the tip, which is why the tip is the place to ask.
      ringTipThickness cutter zr `shouldSatisfy` (> 0)

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
