-- | Render one of the demonstration models to OpenSCAD source.
module Main (main) where

import Data.List (lookup)
import Example
import HasCad
import System.Environment (getProgName)

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      printScad model2
      putTextLn ""
      writeScad "test.scad" model7
    [name] -> withModel name printScad
    [name, out] -> withModel name (writeScad out)
    _ -> usage

withModel :: String -> (Form' -> IO a) -> IO ()
withModel name k = case lookup (toText name) models of
  Just m -> void (k m)
  Nothing -> usage

usage :: IO ()
usage = do
  prog <- getProgName
  die . toString $
    unlines
      ( ("usage: " <> toText prog <> " [MODEL [OUTPUT.scad]]")
          : "models:"
          : fmap (("  " <>) . fst) models
      )
