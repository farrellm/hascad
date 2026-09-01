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
* `herringbone` derives its own slice count from the ambient `$fa` and `$fs`
  instead of leaving it to whatever `slices` happened to be in scope.  It draws
  a 45° helix, so a small gear twists a long way -- the Work gearbox's planets
  turn 50.93° over each half -- and at `slices = 10` that put more rotation
  between one slice and the next than a tooth spans at its tip, which rendered
  the teeth as a stack of torn scales.  An explicit `slices` still wins.
* Teeth may be profile shifted.  `Involute` carries a `shift`, in modules,
  which moves that gear's root and tip out and fattens its tooth at the pitch
  circle without touching the pitch or base circles; `defaultInvolute` sets it
  to zero, so an unshifted model renders exactly as before.  `minShift` gives
  the least shift that avoids undercutting, and `tipThickness` the land left at
  the tip, which is what bounds a shift from above.  Added `shifted`,
  `toothThickness`, `invol`, `invInvol` and `workingAngle`.
* `Planetary` carries `sunShift` and `planetShift`, and `planetary` lays the
  gearbox out at the centre distance the shifts imply rather than at the
  reference one.  The ring's shift is derived, not given: a planet's two meshes
  are one physical gap, so both must run at the same working angle, which
  forces `ringShift = sunShift + 2 planetShift` — the identity `ringTeeth`
  already obeys in teeth.  Shifting also throws the root clearance out, by
  `tipShortening` modules: an external mesh loses that much, so the sun and the
  planets each give it back off the tip, while an internal mesh gains it, so
  the ring is cut the other way — its root back over the planets' already
  shortened tips, and its own teeth a shortening further in.  Added
  `ringShift`, `centerDistance` and `tipShortening`.
* Added `fragments` and `extrudeSlices` to `Graphics.Scad.Facet`, which
  reimplement OpenSCAD's own fragment rule for the geometry hascad has to
  resolve itself, and `askFacet` for reading the settings in scope.
