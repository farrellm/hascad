-- | Involute spur gears, and herringbone planetary gearboxes built from them.
--
-- Gears are sized by tooth count, not by radius: the pitch diameter of a gear
-- is @module * n@, so only whole tooth counts describe a gear whose teeth
-- close the circle.  Every radius is derived — see 'pitchRadius' and friends.
--
-- Teeth may be profile shifted: cutting a gear with the rack held 'shift'
-- modules out fattens the tooth at the pitch circle and lifts the root clear
-- of the point where the rack would otherwise gouge the flank, which is what
-- makes tooth counts below 'minShift' possible at all.  A shift is a property
-- of one gear, not of the mesh; what the mesh sees is the sum of the two, and
-- when that sum is not zero the pair runs at a wider centre distance and a
-- steeper 'workingAngle'.
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
--
-- The ring's shift is not free either.  A planet's two meshes share one
-- physical centre distance, and the reference distances already agree because
-- @ringTeeth - planetTeeth == sunTeeth + planetTeeth@; so both meshes must run
-- at the same 'workingAngle', which forces @ringShift = sunShift + 2
-- planetShift@ — the same identity as 'ringTeeth', in shifts.  One solve for
-- the working angle therefore settles the whole gearbox.  Shifting takes
-- 'tipShortening' modules off the clearance an external mesh has at the root
-- but adds it to an internal one, so 'planetary' shortens the sun and the
-- planets and leaves the ring alone.
module Graphics.Scad.Gear
  ( -- * Tooth form
    Involute (..),
    defaultInvolute,
    shifted,
    internal,
    minShift,

    -- * Derived radii
    pitchRadius,
    baseRadius,
    rootRadius,
    tipRadius,
    toothThickness,
    tipThickness,

    -- * Meshing
    invol,
    invInvol,
    workingAngle,

    -- * Gears
    flank,
    tooth,
    gear,

    -- * Gearboxes
    Planetary (..),
    ringTeeth,
    ringShift,
    centerDistance,
    tipShortening,
    planetary,
    herringbone,
  )
where

import Effectful (Eff)
import Graphics.Scad

-- | The shape of a tooth.  'pressureAngle' and 'module'' are shared by every
-- gear that meshes; the rest, 'shift' especially, are that one gear's own.
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
    -- | Profile shift, in modules: how far out the rack that cuts this gear is
    -- held.  Moves the root and the tip out by as much, and fattens the tooth
    -- at the pitch circle; leaves the pitch and base circles alone.
    shift :: Double,
    -- | Number of straight segments the involute flank is drawn with.
    nSegment :: Int
  }
  deriving stock (Show, Eq)

-- | ISO full-depth teeth of the given module: a 20° pressure angle, an
-- addendum of one module and a dedendum of 1.25, and no profile shift.
defaultInvolute :: Double -> Involute
defaultInvolute m =
  Involute
    { pressureAngle = pi * 20 / 180,
      module' = m,
      addendum = m,
      dedendum = 1.25 * m,
      shift = 0,
      nSegment = 7
    }

-- | The same tooth form, profile shifted by @x@ modules.
shifted :: Double -> Involute -> Involute
shifted x i = i {shift = x}

-- | The cutter for an internal gear: swapping addendum and dedendum turns the
-- teeth into the tooth spaces of the ring they are subtracted from.
--
-- The profile shift passes through untouched, which is worth being clear
-- about, because the cutter's teeth are the ring's spaces and it is tempting
-- to negate it.  The convention an internal mesh is reckoned in already
-- accounts for the swap: a shifted internal gear has the *thinner* tooth, so
-- its space is exactly the tooth an equally shifted external cutter carries.
-- Negating it puts the ring a whole tooth out and nothing meshes.
internal :: Involute -> Involute
internal i = i {addendum = i.dedendum, dedendum = i.addendum}

-- | The least profile shift that keeps the rack from undercutting the flank of
-- an @n@-tooth gear: the rack's tip must not dip below the point where the
-- line of action leaves the base circle.  Negative once the gear is big enough
-- not to need a shift at all — at ISO full depth that is from 18 teeth up.
minShift :: Involute -> Int -> Double
minShift i n =
  let sinA = sin i.pressureAngle
   in i.addendum / i.module' - fromIntegral n * sinA * sinA / 2

-- | The layout of a planetary gearbox.
data Planetary = Planetary
  { -- | Outer radius of the ring blank.
    rOuter :: Double,
    -- | Teeth on the sun.
    sunTeeth :: Int,
    -- | Teeth on each planet.
    planetTeeth :: Int,
    -- | Profile shift on the sun, in modules.
    sunShift :: Double,
    -- | Profile shift on each planet, in modules.  The ring's is not free —
    -- see 'ringShift'.
    planetShift :: Double,
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

-- | Profile shift on the ring, fixed by the two meshes of a planet having to
-- run at one centre distance: @sunShift + 2 planetShift@, the same identity
-- 'ringTeeth' obeys in teeth.
ringShift :: Planetary -> Double
ringShift p = p.sunShift + 2 * p.planetShift

-- | The involute function, @tan a - a@: the angle a taut string subtends as it
-- unwinds to the point where it leaves the base circle at the angle @a@.
invol :: Double -> Double
invol alpha = tan alpha - alpha

-- | Inverse of 'invol', by Newton from the usual cube-root guess.  The
-- derivative is @tan^2@, so a handful of steps land on the double; twenty is
-- generous.  Involutes below @invol 0 == 0@ describe no angle at all, and are
-- clamped rather than chased off to a NaN.
invInvol :: Double -> Double
invInvol y
  | y <= 0 = 0
  | otherwise = go (20 :: Int) ((3 * y) ** (1 / 3))
  where
    go 0 alpha = alpha
    go k alpha =
      let t = tan alpha
          d = (y - (t - alpha)) / (t * t)
       in if abs d < 1e-15 then alpha + d else go (k - 1) (alpha + d)

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
rootRadius i n = pitchRadius i n - i.dedendum + i.shift * i.module'

-- | Radius of the top of the teeth.
tipRadius :: Involute -> Int -> Double
tipRadius i n = pitchRadius i n + i.addendum + i.shift * i.module'

-- | Arc thickness of a tooth at the pitch circle.  Half the circular pitch
-- unshifted; a profile shift adds @2 x m tan a@ to it, which is the whole
-- point of one.
toothThickness :: Involute -> Double
toothThickness i = i.module' * (pi / 2 + 2 * i.shift * tan i.pressureAngle)

-- | Arc thickness of a tooth at the tip: the land left between the two flanks
-- once they have converged.  The upper bound on a shift, as 'minShift' is the
-- lower one — push far enough and the flanks meet and the tooth comes to a
-- point, at which this passes through zero and then turns negative.
tipThickness :: Involute -> Int -> Double
tipThickness i n =
  let rTip = tipRadius i n
      alphaTip = acos (baseRadius i n / rTip)
   in rTip
        * ( toothThickness i / pitchRadius i n
              + 2 * (invol i.pressureAngle - invol alphaTip)
          )

-- | The pressure angle a mesh actually runs at, given the total of its two
-- tooth counts and the total of its two profile shifts.  For an internal pair
-- both totals are differences instead: @nRing - nPlanet@ and
-- @ringShift - planetShift@.
--
-- Shifting a pair by more than zero altogether prises the gears apart: they
-- can only stay in mesh by rolling on wider pitch circles, which is the same
-- as saying they meet at a steeper angle.
workingAngle :: Involute -> Int -> Double -> Double
workingAngle i n x
  | x == 0 = i.pressureAngle
  | otherwise =
      invInvol
        ( invol i.pressureAngle
            + 2 * x * tan i.pressureAngle / fromIntegral n
        )

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
      -- Half of what the profile shift adds to the tooth's angular thickness.
      -- Backing the flank off by it, rather than opening out the hull below,
      -- keeps the tooth centred where an unshifted one sits -- so a shift
      -- fattens a tooth without moving it, and the mesh phasing 'planetary'
      -- works out stays good.
      dPhi = 2 * i.shift * tan i.pressureAngle / fromIntegral n
      c = rotate2d (-(alphaRef + dPhi)) (convex (flank i n))
      -- Half the circular pitch, as an angle: the tooth is that thick at the
      -- pitch circle, and the space between teeth just as wide.  The flanks
      -- land at -dPhi and theta + dPhi, so the tooth is thicker than that by
      -- 2 dPhi, as 'toothThickness' says it should be.
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
herringbone height hand r m = do
  f <- askFacet
  let eps = 1e-5
      height' = height + eps
      twist = 0.5 * hand * height' / r
      -- Slices are this extrude's fragments: the cross-section sweeps
      -- @|twist| * r@ of arc as it climbs -- which, for the 45 degree helix
      -- this draws, is just half the height, whatever the radius -- and $fs
      -- bounds how much of that arc one slice may cover.
      n = extrudeSlices f twist r
  slices n $
    (\c -> [c, mirror (V3 0 0 1) c])
      ## translate
        (V3 0 0 (-eps / 2))
        (linearExtrude (0.5 * height') False 10 twist m)

-- | Distance from the sun's axis to a planet's.  The reference distance
-- @rSun + rPlanet@ when nothing is shifted, and wider than it when the sun and
-- planet shifts add up to more than zero.
--
-- A planet's other mesh comes out at this same distance for free: the ring is
-- @ringTeeth@ teeth and @ringShift@ modules precisely so that it does.
centerDistance :: Involute -> Planetary -> Double
centerDistance i p =
  let nMesh = p.sunTeeth + p.planetTeeth
      xMesh = p.sunShift + p.planetShift
      a = pitchRadius i p.sunTeeth + pitchRadius i p.planetTeeth
   in if xMesh == 0
        then a
        else a * cos i.pressureAngle / cos (workingAngle i nMesh xMesh)

-- | How far, in modules, a shifted mesh throws its root clearance out.
--
-- Shifting a pair by @x@ altogether pushes their tips out by @x@ modules but
-- their centres apart by only @y@ of them.  An external mesh loses the
-- difference from the clearance it keeps at the root, so the sun and the
-- planets each give this much back off the tip.  An internal mesh has the
-- opposite problem: it *gains* clearance, and the ring has to be cut to close
-- the gap again rather than opened up.  Both of the ring's own surfaces are
-- out, and by different amounts -- see 'planetary'.
tipShortening :: Involute -> Planetary -> Double
tipShortening i p =
  let a = pitchRadius i p.sunTeeth + pitchRadius i p.planetTeeth
      y = (centerDistance i p - a) / i.module'
   in p.sunShift + p.planetShift - y

-- | A planetary gearbox: a ring gear, a sun gear, and planets phased so their
-- teeth mesh with both.
--
-- The three gears are cut from @i@ with the shifts the 'Planetary' names, so
-- any 'shift' on @i@ itself is ignored.
planetary :: (HasScad es) => Involute -> Planetary -> Eff es Form
planetary i p =
  let nRing = ringTeeth p
      rSun = pitchRadius i p.sunTeeth
      rPlanet = pitchRadius i p.planetTeeth
      rRing = pitchRadius i nRing
      dTip = tipShortening i p * i.module'
      -- Both flanks of an external mesh have to give up the same ground, so
      -- the shortening comes off the sun and the planets alike.
      short j = j {addendum = j.addendum - dTip}
      iSun = short (shifted p.sunShift i)
      iPlanet = short (shifted p.planetShift i)
      -- The ring is the internal mesh, so its two surfaces are both too far
      -- off the planet rather than too close, and by different amounts.  Its
      -- root is cut by the cutter's tip, and has only the planet's already
      -- shortened tip to clear, so it comes in by the shortening twice over --
      -- once for the shift the centres did not take up, once more for the
      -- shortening itself.  Its own teeth are cut by the cutter's root, and
      -- have to reach a shortening further in to sit the same clearance over
      -- the planet's root.  Leave either alone and the planets rattle around
      -- inside a ring they barely touch.
      iRing =
        let c = internal (shifted (ringShift p) i)
         in c {addendum = c.addendum - 2 * dTip, dedendum = c.dedendum + dTip}
      sun = gear iSun p.sunTeeth
      ring = circle p.rOuter <-> gear iRing nRing
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
              -- Out to where the shifted meshes actually put it, which is the
              -- reference distance only when nothing is shifted.
              . translate (V3 (centerDistance i p) 0 0)
              -- The helix is still measured against the pitch radius: a shift
              -- moves neither pitch circle, and the two halves have to cancel.
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
               gear iPlanet p.planetTeeth
           )
