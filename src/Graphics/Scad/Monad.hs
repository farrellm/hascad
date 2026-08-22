-- | The effects a model is built in: a reader carrying the ambient 'Facet',
-- and a state hoisting repeated module bodies into shared declarations.
module Graphics.Scad.Monad
  ( Scad,
    HasScad,
    Shape',
    Form',
    runScad,
    fa,
    fs,
    fn,
    slices,
    smodule,
    (#),
    (##),
  )
where

import Data.Map.Strict qualified as M
import Effectful (Eff, runPureEff, (:>))
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Graphics.Scad.Dimension (KnownDim)
import Graphics.Scad.Facet (Facet (..), defaultFacet)
import Graphics.Scad.Model

-- | Everything the DSL needs.
type HasScad es =
  ( Reader.Reader Facet :> es,
    State.State ModuleTable :> es
  )

-- | A concrete stack satisfying 'HasScad', for models that need nothing else.
type Scad = Eff '[Reader.Reader Facet, State.State ModuleTable]

type Shape' = Scad Shape

type Form' = Scad Form

runScad :: Scad a -> (a, ModuleTable)
runScad = runPureEff . State.runState mempty . Reader.runReader defaultFacet

-- | Minimum angle of a fragment, in degrees (OpenSCAD's @$fa@).
fa :: (Reader.Reader Facet :> es) => Double -> Eff es a -> Eff es a
fa x = Reader.local (\f -> f {fa = Just x})

-- | Minimum size of a fragment (OpenSCAD's @$fs@).
fs :: (Reader.Reader Facet :> es) => Double -> Eff es a -> Eff es a
fs x = Reader.local (\f -> f {fs = Just x})

-- | Number of fragments in a full circle (OpenSCAD's @$fn@).
fn :: (Reader.Reader Facet :> es) => Double -> Eff es a -> Eff es a
fn x = Reader.local (\f -> f {fn = Just x})

-- | Number of intermediate slices in a twisted @linear_extrude@.
slices :: (Reader.Reader Facet :> es) => Int -> Eff es a -> Eff es a
slices x = Reader.local (\f -> f {slices = Just x})

-- | Hoist @body@ into a top-level module and call it with @children@.
--
-- Structurally equal bodies are emitted once and shared, which is what keeps
-- output for things like a gear's teeth small.
smodule ::
  forall c e es.
  (KnownDim c, KnownDim e, HasScad es) =>
  [Eff es (Model e)] ->
  Eff es (Model c) ->
  Eff es (Model e)
smodule body children = do
  b <- fmap someModel <$> sequenceA body
  table <- State.get
  i <- case M.lookup b table of
    Just i -> pure i
    Nothing -> do
      let i = M.size table
      State.put (M.insert b i table)
      pure i
  Apply (moduleName i) . someModel <$> children

infixr 1 ##

-- | @f ## c@ builds a module whose body is @f children@ and applies it to the
-- model @c@.  The argument passed to @f@ stands for OpenSCAD's @children()@.
(##) ::
  (KnownDim c, KnownDim e, HasScad es) =>
  (Eff es (Model c) -> [Eff es (Model e)]) ->
  Eff es (Model c) ->
  Eff es (Model e)
f ## c = smodule (f (pure Children)) c

infixr 1 #

-- | Single-statement '##'.
(#) ::
  (KnownDim c, KnownDim e, HasScad es) =>
  (Eff es (Model c) -> Eff es (Model e)) ->
  Eff es (Model c) ->
  Eff es (Model e)
f # c = (\x -> [f x]) ## c
