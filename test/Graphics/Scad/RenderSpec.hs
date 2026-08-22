module Graphics.Scad.RenderSpec (spec) where

import Data.Text qualified as T
import Graphics.Scad
import Test.Hspec

shape :: Shape' -> Text
shape = renderText

form :: Form' -> Text
form = renderText

spec :: Spec
spec = do
  describe "two-dimensional primitives" $ do
    it "circle" $
      shape (circle 3) `shouldBe` "circle(3.0);"
    it "square" $
      shape (square 2) `shouldBe` "square(2.0, center = true);"
    it "square'" $
      shape (square' 2) `shouldBe` "square(2.0, center = false);"
    it "rectangle" $
      shape (rectangle (V2 1 2)) `shouldBe` "square([1.0, 2.0], center = true);"
    it "convex" $
      shape (convex [V2 0 0, V2 1 0, V2 0 1])
        `shouldBe` "polygon([[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]]);"
    it "polygon with paths and convexity wraps past 80 columns" $
      shape (polygon [V2 0 0, V2 1 0, V2 0 1] [[0, 1, 2]] (Just 1))
        `shouldBe` T.intercalate
          "\n"
          [ "polygon( [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0]]",
            "       , paths = [[0, 1, 2]]",
            "       , convexity = 1 );"
          ]

  describe "three-dimensional primitives" $ do
    it "sphere" $
      form (sphere 5) `shouldBe` "sphere(5.0);"
    it "cube" $
      form (cube 2) `shouldBe` "cube(2.0, center = true);"
    it "box" $
      form (box (V3 1 2 3)) `shouldBe` "cube([1.0, 2.0, 3.0], center = true);"
    it "cylinder" $
      form (cylinder 10 2)
        `shouldBe` "cylinder(h = 10.0, r = 2.0, center = true);"
    it "cylinder2" $
      form (cylinder2 10 (2, 3))
        `shouldBe` "cylinder(h = 10.0, r1 = 2.0, r2 = 3.0, center = true);"

  describe "tessellation" $ do
    it "$fn applies to the enclosed primitives" $
      shape (fn 8 (circle 3)) `shouldBe` "circle(3.0, $fn = 8.0);"
    it "$fa and $fs stack" $
      shape (fa 6 (fs 0.5 (circle 3)))
        `shouldBe` "circle(3.0, $fa = 6.0, $fs = 0.5);"
    it "settings do not escape their scope" $
      shape (union [fn 8 (circle 1), circle 2])
        `shouldBe` T.unlines ["union() {", "  circle(1.0, $fn = 8.0);", "  circle(2.0);"] <> "}"

  describe "moving between dimensions" $ do
    it "linear_extrude" $
      form (linearExtrude 5 False 10 0 (square 2))
        `shouldBe` "linear_extrude(height = 5.0, center = false, convexity = 10, \
                   \twist = 0.0) {\n  square(2.0, center = true);\n}"
    it "linear_extrude picks up slices" $
      form (slices 4 (linearExtrude 5 False 10 0 (square 2)))
        `shouldBe` T.intercalate
          "\n"
          [ "linear_extrude( height = 5.0",
            "              , center = false",
            "              , convexity = 10",
            "              , twist = 0.0",
            "              , slices = 4 ) {",
            "  square(2.0, center = true);",
            "}"
          ]
    it "project" $
      shape (project (cube 2))
        `shouldBe` "projection(cut = false) {\n  cube(2.0, center = true);\n}"
    it "cut" $
      shape (cut (cube 2))
        `shouldBe` "projection(cut = true) {\n  cube(2.0, center = true);\n}"
    it "offset by radius" $
      shape (offsetR 1 False (square 2))
        `shouldBe` "offset(r = 1.0, chamfer = false) {\n  square(2.0, center = true);\n}"
    it "offset by delta" $
      shape (offsetDelta 1 True (square 2))
        `shouldBe` "offset(delta = 1.0, chamfer = true) {\n  square(2.0, center = true);\n}"

  describe "transformations" $ do
    it "translate" $
      form (translate (V3 1 2 3) (cube 2))
        `shouldBe` "translate([1.0, 2.0, 3.0]) {\n  cube(2.0, center = true);\n}"
    it "rotate about an axis" $
      form (rotate (pi / 2) (V3 0 0 1) (cube 2))
        `shouldBe` "rotate(a = 90.0, v = [0.0, 0.0, 1.0]) {\n  cube(2.0, center = true);\n}"
    it "rotate about each axis" $
      form (rotate' (V3 0 0 pi) (cube 2))
        `shouldBe` "rotate(a = [0.0, 0.0, 180.0]) {\n  cube(2.0, center = true);\n}"
    it "rotate2d" $
      shape (rotate2d (pi / 2) (square 2))
        `shouldBe` "rotate(a = [0.0, 0.0, 90.0]) {\n  square(2.0, center = true);\n}"
    it "scale" $
      shape (scale (V2 2 3) (square 2))
        `shouldBe` "scale([2.0, 3.0]) {\n  square(2.0, center = true);\n}"
    it "resize" $
      form (resize (V3 1 2 3) (cube 2))
        `shouldBe` "resize([1.0, 2.0, 3.0]) {\n  cube(2.0, center = true);\n}"
    it "mirror in three dimensions" $
      form (mirror (V3 1 0 0) (cube 2))
        `shouldBe` "mirror([1.0, 0.0, 0.0]) {\n  cube(2.0, center = true);\n}"
    it "mirror in two dimensions still emits a 3-vector" $
      shape (mirror (V2 1 0) (square 2))
        `shouldBe` "mirror([1.0, 0.0, 0.0]) {\n  square(2.0, center = true);\n}"

  describe "booleans" $ do
    it "union" $
      shape (union [circle 1, circle 2])
        `shouldBe` "union() {\n  circle(1.0);\n  circle(2.0);\n}"
    it "flattens nested unions" $
      shape (union [union [circle 1, circle 2], circle 3])
        `shouldBe` "union() {\n  circle(1.0);\n  circle(2.0);\n  circle(3.0);\n}"
    it "intersection" $
      shape (intersection [circle 1, circle 2])
        `shouldBe` "intersection() {\n  circle(1.0);\n  circle(2.0);\n}"
    it "difference subtracts every argument" $
      form (difference (cube 2) [sphere 1, sphere 2])
        `shouldBe` "difference() {\n  cube(2.0, center = true);\n  sphere(1.0);\n  sphere(2.0);\n}"
    it "<-> chains into one difference" $
      shape (circle 3 <-> circle 2 <-> circle 1)
        `shouldBe` "difference() {\n  circle(3.0);\n  circle(2.0);\n  circle(1.0);\n}"
    it "hull" $
      shape (hull [circle 1, circle 2])
        `shouldBe` "hull() {\n  circle(1.0);\n  circle(2.0);\n}"
    it "minkowski" $
      shape (minkowski [circle 1, circle 2])
        `shouldBe` "minkowski() {\n  circle(1.0);\n  circle(2.0);\n}"

  describe "modifiers" $ do
    it "hidden" $ shape (hidden (circle 1)) `shouldBe` "* circle(1.0);"
    it "debug" $ shape (debug (circle 1)) `shouldBe` "# circle(1.0);"
    it "background" $ shape (background (circle 1)) `shouldBe` "% circle(1.0);"

  describe "modules" $ do
    it "hoists a body and applies it to its children" $
      shape ((\c -> translate (V2 1 0) c) # circle 1)
        `shouldBe` T.intercalate
          "\n"
          [ "module mdl_0() {",
            "  translate([1.0, 0.0]) {",
            "    children();",
            "  }",
            "}",
            "mdl_0() {",
            "  circle(1.0);",
            "}"
          ]
    it "emits a multi-statement body" $
      shape ((\c -> [c, translate (V2 1 0) c]) ## circle 1)
        `shouldBe` T.intercalate
          "\n"
          [ "module mdl_0() {",
            "  children();",
            "  translate([1.0, 0.0]) {",
            "    children();",
            "  }",
            "}",
            "mdl_0() {",
            "  circle(1.0);",
            "}"
          ]
    it "shares one declaration between equal bodies" $ do
      let out =
            shape
              ( union
                  [ (\c -> [c, translate (V2 1 0) c]) ## circle 1,
                    (\c -> [c, translate (V2 1 0) c]) ## circle 2
                  ]
              )
      T.count "module mdl_" out `shouldBe` 1
      T.count "mdl_0()" out `shouldBe` 3
    it "gives distinct bodies distinct declarations" $ do
      let out =
            shape
              ( union
                  [ (\c -> [c, translate (V2 1 0) c]) ## circle 1,
                    (\c -> [c, translate (V2 2 0) c]) ## circle 1
                  ]
              )
      T.count "module mdl_" out `shouldBe` 2
      T.count "mdl_1()" out `shouldBe` 2
