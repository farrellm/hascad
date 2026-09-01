# hascad

A Haskell DSL for generating [OpenSCAD](https://openscad.org) models.

Models are indexed by their dimension, so the type checker rejects extruding a
solid or unioning a square with a cube. Repeated geometry is de-duplicated
automatically into OpenSCAD `module` declarations, which keeps output for
things like gear teeth small.

```haskell
import Graphics.Scad

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
- **Sharing** — `#` and `##` lift a Haskell function into an OpenSCAD module;
  see below.

`Graphics.Scad.Gear` builds involute spur gears and herringbone planetary gearboxes
on top of all this. Teeth may be profile shifted, which is what lets tooth counts
below the undercut limit come out with usable flanks; a shifted planetary is laid
out at the wider centre distance its shifts imply. `examples/Example.hs` has
runnable models.

## Modules from functions

`##` turns an ordinary Haskell function into an OpenSCAD `module`. The function
receives the model that will become the module's `children()`, and returns the
statements of its body:

```haskell
quads :: Shape'
quads = (\c -> [rotate2d (n * tau / 4) c | n <- [0 .. 3]]) ## circle 2
```

```openscad
module mdl_0() {
  rotate(a = [0.0, 0.0, 0.0]) {
    children();
  }
  rotate(a = [0.0, 0.0, 90.0]) {
    children();
  }
  rotate(a = [0.0, 0.0, 180.0]) {
    children();
  }
  rotate(a = [0.0, 0.0, 270.0]) {
    children();
  }
}
mdl_0() {
  circle(2.0);
}
```

The function is applied to a placeholder standing for `children()`, so whatever
it does with its argument becomes the body, and the model you pass on the right
becomes the call.

Bodies are compared structurally and emitted once. Apply that same function to a
`square 3` as well and you get a second `mdl_0() { .. }` call site rather than a
second declaration — which is what stops a gear from writing its tooth out once
per tooth.

`#` is the same thing where the body is a single statement.

## Building

```
cabal build
cabal test
```

CI builds with `--flags=+werror`, which turns `-Wall` into `-Werror`. To
reproduce that locally:

```
cabal build all --enable-tests --flags=+werror
```
