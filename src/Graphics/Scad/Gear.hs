{-# LANGUAGE MultiWayIf #-}

-- | Involute spur gears, and herringbone planetary gearboxes built from them.
module Graphics.Scad.Gear
  ( Involute (..),
    Planetary (..),
    tooth,
    gear,
    planetary,
  )
where

import Effectful (Eff)
import Graphics.Scad

-- | The shape of a tooth, shared by every gear that meshes.
data Involute = Involute
  { pressureAngle :: Double,
    module' :: Double,
    addendum :: Double,
    dedendum :: Double,
    nSegment :: Int
  }
  deriving stock (Show)

-- | The layout of a planetary gearbox.
data Planetary = Planetary
  { rOuter :: Double,
    rSun :: Double,
    rPlanet :: Double,
    nPlanet :: Int,
    planetOffset :: Double
  }
  deriving stock (Show)

invol :: Double -> Double
invol alpha = tan alpha - alpha

baseRadius :: Involute -> Double -> Double
baseRadius i rPitch = cos i.pressureAngle * rPitch

involute :: Involute -> Double -> [V2 Double]
involute i rPitch =
  let rBase = baseRadius i rPitch
      minR = rBase
      maxR = rPitch + i.addendum
      dR = (maxR - minR) / fromIntegral i.nSegment
      rs = [minR + fromIntegral j * dR | j <- [0 .. i.nSegment]]
      as = [acos (rBase / r) | r <- rs]
      xs = [r * cos (invol a) | (r, a) <- zip rs as]
      ys = [r * sin (invol a) | (r, a) <- zip rs as]
   in zipWith V2 xs ys

-- | A single tooth, centered on the positive x-axis.
tooth :: (HasScad es) => Involute -> Double -> Eff es Shape
tooth i rPitch =
  let rBase = baseRadius i rPitch
      alphaRef = invol (acos (rBase / rPitch))
      c = convex (involute i rPitch)
      c' = rotate2d (-alphaRef) c
      pitch = pi * i.module'
      theta = pitch / rPitch / 2
   in (\x -> hull [x, rotate2d theta $ mirror (V2 0 1) x]) # c'

-- | A spur gear of the given pitch radius.
gear :: (HasScad es) => Involute -> Double -> Eff es Shape
gear i rPitch =
  let pitch = pi * i.module'
      theta = pitch / rPitch
      t = tooth i rPitch
      nTeeth = round (2 * rPitch / i.module') :: Int
   in ( \x ->
          circle (rPitch - i.dedendum)
            : [rotate2d (theta * fromIntegral n) x | n <- [0 .. nTeeth - 1]]
      )
        ## t

-- | A planetary gearbox of the given height: a ring gear, a sun gear, and
-- planets phased so their teeth mesh with both.
planetary :: (HasScad es) => Involute -> Planetary -> Double -> Eff es Form
planetary i p height =
  let sun = gear i p.rSun
      parity =
        0.5 * fromIntegral (round (2 * p.rPlanet / i.module') `mod` 2 :: Int)
      i' = i {addendum = i.dedendum, dedendum = i.addendum}
      rRing = p.rSun + 2 * p.rPlanet
      ring = circle p.rOuter <-> gear i' rRing
      pitch = pi * i.module'
      -- beta: the angle of omega per tooth cycle
      betaRing = pitch / rRing
      betaSun = pitch / p.rSun -- betaPlanet == betaSun since meshed
      dRing_dOmega = 1 / betaRing
      dSun_dOmega = 1 / betaSun
      theta = pitch / p.rPlanet
      planet omega =
        let (_ :: Int, phaseRing) = properFraction (omega / betaRing)
            (_ :: Int, phasePlnt) = properFraction (parity - omega / betaSun)
            phasePlnt' =
              if
                | phasePlnt < -0.5 -> phasePlnt + 1
                | phasePlnt >= 0.5 -> phasePlnt - 1
                | otherwise -> phasePlnt
            delta = (phasePlnt' - phaseRing) / (dRing_dOmega + dSun_dOmega)
            omega' = omega + 1 * delta
         in rotate' (V3 0 0 omega')
              . translate (V3 (p.rPlanet + p.rSun) 0 0)
              . herringbone (-1) p.rPlanet
              . rotate2d ((theta / 2) + (omega' * p.rSun / p.rPlanet))
   in ( \g ->
          herringbone (-1) rRing ring
            : herringbone 1 p.rSun sun
            : [ planet (tau / fromIntegral p.nPlanet * fromIntegral n) g
              | n <- [0 .. p.nPlanet - 1]
              ]
      )
        ## (mirror (V2 1 0) . offsetR (-p.planetOffset) False $ gear i p.rPlanet)
  where
    -- Two mirrored halves of opposite twist, so axial forces cancel.
    herringbone :: (HasScad es) => Double -> Double -> Eff es Shape -> Eff es Form
    herringbone sgn r m =
      let eps = 1e-5
          height' = height + eps
       in (\c -> [c, mirror (V3 0 0 1) c])
            ## translate
              (V3 0 0 (-eps / 2))
              (linearExtrude (0.5 * height') False 10 (0.5 * sgn * height' / r) m)
