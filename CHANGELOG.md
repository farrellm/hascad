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
