-- | Turning a model into OpenSCAD source.
module Graphics.Scad.Render
  ( render,
    renderText,
    printScad,
    writeScad,
  )
where

import Data.Map.Strict qualified as M
import Graphics.Scad.Dimension (KnownDim)
import Graphics.Scad.Model
import Graphics.Scad.Monad
import Prettyprinter
import Prettyprinter.Render.Text (putDoc, renderStrict)

-- | Render a model, preceded by the module declarations it hoisted, in the
-- order they were created.
render :: (KnownDim d) => Scad (Model d) -> Doc ann
render mdl =
  let (m, table) = runScad mdl
      ms = [Module (moduleName i) b | (b, i) <- sortOn snd (M.toList table)]
   in vcat (fmap pretty ms <> [pretty m])

renderText :: (KnownDim d) => Scad (Model d) -> Text
renderText = renderStrict . layoutSmart defaultLayoutOptions . render

printScad :: (KnownDim d) => Scad (Model d) -> IO ()
printScad = putDoc . render

writeScad :: (KnownDim d) => FilePath -> Scad (Model d) -> IO ()
writeScad f = writeFileText f . renderText
