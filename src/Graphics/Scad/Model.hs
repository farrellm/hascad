{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The OpenSCAD abstract syntax tree, and how it prints.
--
-- 'Model' is indexed by 'Dimension' so that the type checker rejects, say,
-- extruding a solid or unioning a square with a cube.  Constructors are
-- exported so that alternative interpreters can be written; the intended way
-- to build a 'Model' is the DSL re-exported from "Graphics.Scad".
--
-- The pretty-printer is written once, polymorphically in the dimension;
-- 'sdim' is consulted only where the two dimensions genuinely differ.
module Graphics.Scad.Model
  ( Radian (..),
    tau,
    OffsetMode (..),
    Model (..),
    Shape,
    Form,
    SomeModel (..),
    someModel,
    Module (..),
    ModuleTable,
    moduleName,
  )
where

import Graphics.Scad.Dimension
import Graphics.Scad.Facet (Facet (..))
import Graphics.Scad.Vector (V2 (..), V3 (..))
import Prettyprinter

-- | An angle in radians.  OpenSCAD works in degrees; the conversion happens
-- when rendering.
newtype Radian = Radian Double
  deriving stock (Show, Eq, Ord)

-- | One full turn.
tau :: (Floating a) => a
tau = 2 * pi

data OffsetMode
  = OffsetR Double
  | OffsetDelta Double
  deriving stock (Show, Eq, Ord)

data Model d where
  Circle :: Double -> Facet -> Model 'Two
  Square :: Double -> Bool -> Model 'Two
  Rectangle :: V2 Double -> Bool -> Model 'Two
  Polygon :: [V2 Double] -> [[Int]] -> Maybe Int -> Model 'Two
  Sphere :: Double -> Facet -> Model 'Three
  Cube :: Double -> Bool -> Model 'Three
  Box :: V3 Double -> Bool -> Model 'Three
  Cylinder :: Double -> Double -> Bool -> Facet -> Model 'Three
  Cylinder2 :: Double -> Double -> Double -> Bool -> Facet -> Model 'Three
  LinearExtrude ::
    Double -> Bool -> Int -> Radian -> Facet -> Model 'Two -> Model 'Three
  Projection :: Bool -> Model 'Three -> Model 'Two
  Offset :: OffsetMode -> Bool -> Model 'Two -> Model 'Two
  Translate :: V d Double -> Model d -> Model d
  RotateA :: V3 Radian -> Model d -> Model d
  RotateV :: Radian -> V3 Double -> Model d -> Model d
  Scale :: V d Double -> Model d -> Model d
  Resize :: V d Double -> Model d -> Model d
  Mirror :: V d Double -> Model d -> Model d
  Hull :: [Model d] -> Model d
  Minkowski :: [Model d] -> Model d
  Union' :: [Model d] -> Model d
  Intersection' :: [Model d] -> Model d
  Difference :: Model d -> Model d -> Model d
  Hidden :: Model d -> Model d
  Debug :: Model d -> Model d
  Background :: Model d -> Model d
  -- | Call a named module, passing a model as its @children()@.  The
  -- children may have a different dimension than the result.
  Apply :: Text -> SomeModel -> Model d
  Children :: Model d

type Shape = Model 'Two

type Form = Model 'Three

-- | A 'Model' with its dimension erased, so that models of either dimension
-- can share a container.
data SomeModel
  = Model2 (Model 'Two)
  | Model3 (Model 'Three)
  deriving stock (Show, Eq, Ord)

someModel :: forall d. (KnownDim d) => Model d -> SomeModel
someModel m = case sdim @d of
  STwo -> Model2 m
  SThree -> Model3 m

deriving stock instance Show (Model 'Two)

deriving stock instance Show (Model 'Three)

deriving stock instance Eq (Model 'Two)

deriving stock instance Eq (Model 'Three)

deriving stock instance Ord (Model 'Two)

deriving stock instance Ord (Model 'Three)

-- | A top-level @module name() { .. }@ declaration.
data Module = Module Text [SomeModel]
  deriving stock (Show, Eq, Ord)

-- | Module bodies that have been hoisted into declarations, mapped to the
-- index they were assigned.  Equal bodies share one declaration.
type ModuleTable = Map [SomeModel] Int

moduleName :: Int -> Text
moduleName i = "mdl_" <> show i

-- * Pretty-printing

instance Pretty Radian where
  pretty (Radian r) = pretty (180 * r / pi)

prettyVec :: (Foldable f, Pretty a) => f a -> Doc ann
prettyVec = list . fmap pretty . toList

-- | Pretty-print a vector whose width depends on a dimension variable.
prettyV :: forall d a ann. (KnownDim d, Pretty a) => V d a -> Doc ann
prettyV v = case sdim @d of
  STwo -> prettyVec v
  SThree -> prettyVec v

ppBool :: Bool -> Doc ann
ppBool True = "true"
ppBool False = "false"

named :: Doc ann -> Doc ann -> Doc ann
named n v = n <+> "=" <+> v

named' :: (Pretty a) => Doc ann -> Maybe a -> [Doc ann]
named' n = maybe [] (\v -> [named n (pretty v)])

center :: Bool -> Doc ann
center = named "center" . ppBool

facets :: Facet -> [Doc ann]
facets f =
  concat
    [ named' "$fa" f.fa,
      named' "$fs" f.fs,
      named' "$fn" f.fn
    ]

block :: [Doc ann] -> Doc ann
block xs = vcat [nest 2 (vcat (lbrace : xs)), rbrace]

instance forall d. (KnownDim d) => Pretty (Model d) where
  pretty = \case
    Circle r f ->
      "circle" <> align (tupled (pretty r : facets f)) <> ";"
    Square r c ->
      "square" <> align (tupled [pretty r, center c]) <> ";"
    Rectangle r c ->
      "square" <> align (tupled [prettyVec r, center c]) <> ";"
    Polygon vs ps c ->
      let paths =
            if null ps then [] else [named "paths" (list (fmap prettyVec ps))]
       in "polygon"
            <> align
              ( tupled
                  (align (list (fmap prettyVec vs)) : paths <> named' "convexity" c)
                  <> ";"
              )
    Sphere r f ->
      "sphere" <> align (tupled (pretty r : facets f)) <> ";"
    Cube r c ->
      "cube" <> align (tupled [pretty r, center c]) <> ";"
    Box r c ->
      "cube" <> align (tupled [prettyVec r, center c]) <> ";"
    Cylinder h r c f ->
      "cylinder"
        <> align
          ( tupled
              (named "h" (pretty h) : named "r" (pretty r) : center c : facets f)
              <> ";"
          )
    Cylinder2 h r1 r2 c f ->
      "cylinder"
        <> align
          ( tupled
              ( named "h" (pretty h)
                  : named "r1" (pretty r1)
                  : named "r2" (pretty r2)
                  : center c
                  : facets f
              )
              <> ";"
          )
    LinearExtrude h c v t f m ->
      "linear_extrude"
        <> align
          ( tupled
              ( named "height" (pretty h)
                  : named "center" (ppBool c)
                  : named "convexity" (pretty v)
                  : named "twist" (pretty t)
                  : named' "slices" f.slices
                    <> facets f
              )
          )
        <+> block [pretty m]
    Projection c m ->
      "projection" <> parens (named "cut" (ppBool c)) <+> block [pretty m]
    Offset (OffsetR r) c m ->
      "offset"
        <> align (tupled [named "r" (pretty r), named "chamfer" (ppBool c)])
        <+> block [pretty m]
    Offset (OffsetDelta dl) c m ->
      "offset"
        <> align (tupled [named "delta" (pretty dl), named "chamfer" (ppBool c)])
        <+> block [pretty m]
    Translate v m -> "translate" <> parens (prettyV @d v) <+> block [pretty m]
    RotateA a m -> "rotate" <> parens (named "a" (prettyVec a)) <+> block [pretty m]
    RotateV a v m ->
      "rotate"
        <> tupled [named "a" (pretty a), named "v" (prettyVec v)]
        <+> block [pretty m]
    Scale v m -> "scale" <> parens (prettyV @d v) <+> block [pretty m]
    Resize v m -> "resize" <> parens (prettyV @d v) <+> block [pretty m]
    -- OpenSCAD's mirror is always given a 3-vector, even in two dimensions.
    Mirror v m ->
      let v' = case sdim @d of
            STwo -> case v of V2 x y -> prettyVec (V3 x y 0)
            SThree -> prettyVec v
       in "mirror" <> parens v' <+> block [pretty m]
    Hull ms -> "hull()" <+> block (fmap pretty ms)
    Minkowski ms -> "minkowski()" <+> block (fmap pretty ms)
    Union' ms -> "union()" <+> block (fmap pretty ms)
    Intersection' ms -> "intersection()" <+> block (fmap pretty ms)
    Difference x (Union' ys) -> "difference()" <+> block (fmap pretty (x : ys))
    Difference x y -> "difference()" <+> block (fmap pretty [x, y])
    Hidden x -> "*" <+> pretty x
    Debug x -> "#" <+> pretty x
    Background x -> "%" <+> pretty x
    Apply n m -> pretty n <> "()" <+> block [pretty m]
    Children -> "children();"

instance Pretty SomeModel where
  pretty (Model2 m) = pretty m
  pretty (Model3 m) = pretty m

instance Pretty Module where
  pretty (Module n ms) =
    "module" <+> pretty n <> "()" <+> block (fmap pretty ms)
