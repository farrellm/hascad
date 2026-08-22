-- | Involute spur gears, and herringbone planetary gearboxes built from them.
--
-- Gears are sized by tooth count, not by radius: the pitch diameter of a gear
-- is @module * n@, so only whole tooth counts describe a gear whose teeth
-- close the circle.  Every radius is derived — see 'pitchRadius' and friends.
--
-- Two approximations are worth knowing about.  Inside the base circle the
-- involute is undefined, so 'flank' drops a radial line to the root rather
-- than the trochoid a real hob would cut; the tooth is slightly fat there.
-- And an internal gear is cut with an external cutter of swapped addendum and
-- dedendum ('internal'), which ignores the tip interference a proper internal
-- profile would account for.
--
-- In a 'Planetary' the ring's tooth count is not free: meshing forces
-- @rRing = rSun + 2 rPlanet@, hence 'ringTeeth'.  Planets land on exactly even
-- spacing only when @('sunTeeth' + 'ringTeeth') \`mod\` 'nPlanet' == 0@; when
-- it does not hold, 'planetary' nudges each planet along its orbit to the
-- nearest angle where both of its meshes agree.
module Graphics.Scad.Gear
  ( -- * Tooth form
    Involute (..),
    defaultInvolute,
    internal,

    -- * Derived radii
    pitchRadius,
    baseRadius,
    rootRadius,
    tipRadius,

    -- * Gears
    flank,
    tooth,
    gear,

    -- * Gearboxes
    Planetary (..),
    ringTeeth,
    planetary,
    herringbone,
  )
where

import Effectful (Eff)
import Graphics.Scad

-- | The shape of a tooth, shared by every gear that meshes.
data Involute = Involute
  { -- | Pressure angle, in radians.
    pressureAngle :: Double,
    -- | Module: pitch diameter per tooth.  Two gears mesh only if they agree
    -- on this.
    module' :: Double,
    -- | How far the tooth rises above the pitch circle.
    addendum :: Double,
    -- | How far the tooth space falls below it.  Conventionally a little more
    -- than the addendum, to leave clearance at the root.
    dedendum :: Double,
    -- | Number of straight segments the involute flank is drawn with.
    nSegment :: Int
  }
  deriving stock (Show, Eq)

-- | ISO full-depth teeth of the given module: a 20° pressure angle, an
-- addendum of one module and a dedendum of 1.25.
defaultInvolute :: Double -> Involute
defaultInvolute m =
  Involute
    { pressureAngle = pi * 20 / 180,
      module' = m,
      addendum = m,
      dedendum = 1.25 * m,
      nSegment = 7
    }

-- | The cutter for an internal gear: swapping addendum and dedendum turns the
-- teeth into the tooth spaces of the ring they are subtracted from.
internal :: Involute -> Involute
internal i = i {addendum = i.dedendum, dedendum = i.addendum}

-- | The layout of a planetary gearbox.
data Planetary = Planetary
  { -- | Outer radius of the ring blank.
    rOuter :: Double,
    -- | Teeth on the sun.
    sunTeeth :: Int,
    -- | Teeth on each planet.
    planetTeeth :: Int,
    -- | How many planets.
    nPlanet :: Int,
    -- | Radial clearance taken off each planet, to leave room for a printed
    -- part to actually turn.
    backlash :: Double,
    -- | Height of the gearbox.
    height :: Double
  }
  deriving stock (Show, Eq)

-- | Teeth on the ring, fixed by the meshing constraint
-- @rRing = rSun + 2 rPlanet@.
ringTeeth :: Planetary -> Int
ringTeeth p = p.sunTeeth + 2 * p.planetTeeth

invol :: Double -> Double
invol alpha = tan alpha - alpha

-- | The signed distance to the nearest whole number, in @[-0.5, 0.5)@.
wrap :: Double -> Double
wrap x =
  let (_ :: Int, f) = properFraction x
      f' = if f < 0 then f + 1 else f
   in if f' >= 0.5 then f' - 1 else f'

-- | Radius of the circle the teeth roll on: @module * n / 2@.
pitchRadius :: Involute -> Int -> Double
pitchRadius i n = i.module' * fromIntegral n / 2

-- | Radius of the circle the involute is generated from.
baseRadius :: Involute -> Int -> Double
baseRadius i n = cos i.pressureAngle * pitchRadius i n

-- | Radius of the bottom of the tooth spaces.
rootRadius :: Involute -> Int -> Double
rootRadius i n = pitchRadius i n - i.dedendum

-- | Radius of the top of the teeth.
tipRadius :: Involute -> Int -> Double
tipRadius i n = pitchRadius i n + i.addendum

-- | One flank of a tooth, running from the root out to the tip, and meeting
-- the base circle on the positive x-axis.
flank :: Involute -> Int -> [V2 Double]
flank i n =
  let rBase = baseRadius i n
      rRoot = rootRadius i n
      dR = (tipRadius i n - rBase) / fromIntegral i.nSegment
      p r = let a = invol (acos (rBase / r)) in V2 (r * cos a) (r * sin a)
      -- The involute is undefined inside the base circle.  Where the root
      -- falls there, extend the flank radially down to it, so that the tooth
      -- still reaches the root disc it is unioned onto.
      fillet = [V2 rRoot 0 | rRoot < rBase]
   in fillet <> [p (rBase + fromIntegral j * dR) | j <- [0 .. i.nSegment]]

-- | A single tooth of an @n@-tooth gear, centered on the positive x-axis.
tooth :: (HasScad es) => Involute -> Int -> Eff es Shape
tooth i n =
  let -- Where on the flank the pitch circle falls, so it can be rotated to
      -- the x-axis.
      alphaRef = invol i.pressureAngle
      c = rotate2d (-alphaRef) (convex (flank i n))
      -- Half the circular pitch, as an angle: the tooth is that thick at the
      -- pitch circle, and the space between teeth just as wide.
      theta = pi / fromIntegral n
   in (\x -> hull [x, rotate2d theta $ mirror (V2 0 1) x]) # c

-- | A spur gear with @n@ teeth.
gear :: (HasScad es) => Involute -> Int -> Eff es Shape
gear i n =
  let theta = tau / fromIntegral n
   in ( \x ->
          circle (rootRadius i n)
            : [rotate2d (theta * fromIntegral k) x | k <- [0 .. n - 1]]
      )
        ## tooth i n

-- | Two mirrored halves of opposite twist, so axial forces cancel.  @hand@ is
-- the sense of the twist, @1@ or @-1@; @r@ is the radius the 45° helix is
-- measured against.
herringbone ::
  (HasScad es) => Double -> Double -> Double -> Eff es Shape -> Eff es Form
herringbone height hand r m =
  let eps = 1e-5
      height' = height + eps
   in (\c -> [c, mirror (V3 0 0 1) c])
        ## translate
          (V3 0 0 (-eps / 2))
          (linearExtrude (0.5 * height') False 10 (0.5 * hand * height' / r) m)

-- | A planetary gearbox: a ring gear, a sun gear, and planets phased so their
-- teeth mesh with both.
planetary :: (HasScad es) => Involute -> Planetary -> Eff es Form
planetary i p =
  let nRing = ringTeeth p
      rSun = pitchRadius i p.sunTeeth
      rPlanet = pitchRadius i p.planetTeeth
      rRing = pitchRadius i nRing
      sun = gear i p.sunTeeth
      ring = circle p.rOuter <-> gear (internal i) nRing
      -- An odd planet sits half a tooth out of phase with itself across the
      -- line joining its two meshes.
      parity = 0.5 * fromIntegral (p.planetTeeth `mod` 2)
      -- A full turn of the carrier advances the planet's mesh phase by this
      -- many teeth, counting both of its meshes together; so one tooth of
      -- phase is tau / nMesh of orbit.
      nMesh = fromIntegral (p.sunTeeth + nRing)
      -- How far a planet turns on its own axis per radian of orbit.
      spin = fromIntegral p.sunTeeth / fromIntegral p.planetTeeth
      planet omega =
        -- Nudge the planet along its orbit to the nearest angle at which both
        -- meshes agree; the nudge is never more than half a tooth.
        let omega' = omega + wrap (parity - omega * nMesh / tau) * tau / nMesh
         in rotate' (V3 0 0 omega')
              . translate (V3 (rPlanet + rSun) 0 0)
              . herringbone p.height (-1) rPlanet
              . rotate2d (pi / fromIntegral p.planetTeeth + omega' * spin)
   in ( \g ->
          herringbone p.height (-1) rRing ring
            : herringbone p.height 1 rSun sun
            : [ planet (tau * fromIntegral k / fromIntegral p.nPlanet) g
              | k <- [0 .. p.nPlanet - 1]
              ]
      )
        ## ( mirror (V2 1 0) . offsetR (-p.backlash) False $
               gear i p.planetTeeth
           )
