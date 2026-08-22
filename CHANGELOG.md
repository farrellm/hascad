# Revision history for hascad

## 0.1.0.0 -- unreleased

* Port of the `scad` package: a dimension-indexed DSL for generating OpenSCAD
  source, with automatic sharing of repeated geometry, and involute spur and
  planetary gears.
* Effects are handled by `effectful` rather than `polysemy`.
* Fixed two renderings that OpenSCAD rejected: `rotate` about an axis in two
  dimensions emitted `rotate([a = .., v = ..])`, and a single-element `paths`
  argument to `polygon` was emitted unwrapped.
* Module declarations are emitted in the order they are created, rather than in
  an order derived from comparing their bodies.
* `Graphics.Scad.Gear` describes gears by tooth count rather than pitch radius.
  `tooth` and `gear` take an `Int`; `Planetary` carries `sunTeeth`,
  `planetTeeth`, `backlash` and `height` in place of `rSun`, `rPlanet`,
  `planetOffset` and the height argument to `planetary`, and the ring's tooth
  count is derived by `ringTeeth`.  A radius that was not an exact multiple of
  half the module used to yield teeth that did not close the circle.
* Fixed teeth that were disconnected from the root of the gear whenever the
  base circle fell outside the root circle, which is the case at conventional
  pressure angles and tooth counts.  The flank is now extended radially inward
  to the root.
* Each planet is nudged to the *nearest* angle at which both of its meshes
  agree, rather than to one up to a tooth and a half away.
* Added `defaultInvolute`, `internal`, `herringbone`, `flank` and the derived
  radii `pitchRadius`, `baseRadius`, `rootRadius` and `tipRadius`.
