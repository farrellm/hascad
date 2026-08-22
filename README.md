# hascad

A Haskell DSL for generating [OpenSCAD](https://openscad.org) models.

Models are indexed by their dimension, so the type checker rejects extruding a
solid or unioning a square with a cube. Repeated geometry is de-duplicated
automatically into OpenSCAD `module` declarations, which keeps output for
things like gear teeth small.

```haskell
import HasCad

tube :: Form'
tube = fn 90 (cylinder 20 5 <-> cylinder 21 3)

main :: IO ()
main = writeScad "tube.scad" tube
```

```
$ cabal run hascad -- planetary gearbox.scad
$ openscad -o gearbox.stl gearbox.scad
```

## The language

Models live in the `Scad` monad, which carries the ambient tessellation
settings (`fa`, `fs`, `fn`, `slices`) and the table of shared modules. A model
is a `Shape'` (two dimensional) or a `Form'` (three dimensional), and is turned
into source with `render`, `renderText`, `printScad` or `writeScad`.

- **Primitives** — `circle`, `square`, `rectangle`, `convex`, `polygon`;
  `sphere`, `cube`, `box`, `cylinder`, `cylinder2`. The primed variants
  (`square'`, `cube'`, …) are the un-centered ones.
- **Transformations** — `translate`, `rotate`, `rotate'`, `rotate2d`, `scale`,
  `resize`, `mirror`. Angles are in radians; OpenSCAD's degrees are an output
  detail.
- **Between dimensions** — `linearExtrude`, `project`, `cut`, `offsetR`,
  `offsetDelta`.
- **Combining** — `union`, `intersection`, `difference`, `hull`, `minkowski`,
  and the operators `<+>`, `<#>`, `<->`, which flatten as they chain.
- **Modifiers** — `hidden`, `debug`, `background` (OpenSCAD's `*`, `#`, `%`).
- **Sharing** — `#` and `##` lift a function of `children()` into a module:

  ```haskell
  -- one declaration, applied to a circle
  (\c -> [c, translate (V2 1 0) c]) ## circle 1
  ```

`HasCad.Gear` builds involute spur gears and herringbone planetary gearboxes
on top of all this; `examples/Example.hs` has runnable models.

## Building

```
cabal build
cabal test
```
