{-# LANGUAGE Safe #-}

module Example.Example_2_CountWords (
  main,
) where

import qualified System.Environment as IO
import qualified Text.LambdaOptions as L

options :: L.Options () ()
options = do
  L.addOption (L.kw ()) ()

main :: IO ()
main = do
  args <- IO.getArgs
  case L.runOptions options args of
    Left {}  -> error "Internal logic error."
    Right xs -> print $ length xs

