-- | A Haskell DSL for generating OpenSCAD source.
--
-- Models are built in the 'Scad' monad, which carries the ambient
-- tessellation 'Facet' and de-duplicates repeated geometry into OpenSCAD
-- modules.  A model is a value of type @'Scad' 'Shape'@ (two dimensional) or
-- @'Scad' 'Form'@ (three dimensional) — spelled 'Shape'' and 'Form'' — and is
-- turned into source with 'render', 'printScad' or 'writeScad'.
--
-- > tube :: Form'
-- > tube = fn 90 (cylinder 20 5 <-> cylinder 21 3)
--
-- This module re-exports the DSL.  Import "Graphics.Scad.Model" as well if you want
-- the AST constructors.
module Graphics.Scad
  ( -- * Models
    Dimension (..),
    KnownDim,
    Model,
    Shape,
    Form,
    Shape',
    Form',
    Module,
    V2 (..),
    V3 (..),
    Radian (..),
    tau,

    -- * Building
    Scad,
    HasScad,
    runScad,

    -- * Rendering
    render,
    renderText,
    printScad,
    writeScad,

    -- * Tessellation
    Facet,
    defaultFacet,
    fa,
    fs,
    fn,
    slices,
    askFacet,
    fragments,
    extrudeSlices,

    -- * Two-dimensional primitives
    circle,
    square,
    square',
    rectangle,
    rectangle',
    convex,
    polygon,

    -- * Three-dimensional primitives
    sphere,
    cube,
    cube',
    box,
    box',
    cylinder,
    cylinder',
    cylinder2,
    cylinder2',

    -- * Moving between dimensions
    linearExtrude,
    projection,
    project,
    cut,
    offsetR,
    offsetDelta,

    -- * Transformations
    translate,
    rotate,
    rotate',
    rotate2d,
    scale,
    resize,
    mirror,

    -- * Combining
    SetLike (..),
    Union (..),
    Intersection (..),
    union,
    intersection,
    difference,
    hull,
    minkowski,

    -- * Modifiers
    hidden,
    debug,
    background,

    -- * Sharing
    (#),
    (##),
  )
where

import Graphics.Scad.Boolean
import Graphics.Scad.Dimension (Dimension (..), KnownDim)
import Graphics.Scad.Facet (Facet, defaultFacet, extrudeSlices, fragments)
import Graphics.Scad.Model (Form, Model, Module, Radian (..), Shape, tau)
import Graphics.Scad.Monad
import Graphics.Scad.Primitive
import Graphics.Scad.Render
import Graphics.Scad.Transform
import Graphics.Scad.Vector (V2 (..), V3 (..))
