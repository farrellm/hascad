-- | End-to-end check on a model that exercises most of the language: nested
-- module sharing, extrusion, offsets and booleans.
module HasCad.GoldenSpec (spec) where

import Example (model7)
import HasCad
import Test.Hspec

spec :: Spec
spec = describe "planetary gearbox" $
  it "renders to the golden output" $ do
    expected <- decodeUtf8 <$> readFileBS "test/golden/planetary.scad"
    renderText model7 `shouldBe` expected
