{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

module Text.LambdaOptions.Example.Example_2_Booly (
  main,
) where

import qualified System.Environment as IO
import qualified Text.LambdaOptions as L

type B = L.Booly
  'L.AllowWord
  'L.DisallowLetter
  'L.AllowBit
  ('L.OrCasing 'L.LowerAll 'L.UpperAll)

pattern B :: Bool -> B
pattern B x = L.Booly x

options :: L.Options Bool ()
options = do

  L.addOption
    (L.kw "--and"
    `L.argText` "X Y"
    `L.text` "Logical And.")
    $ \(B x) (B y) -> x && y

  L.addOption
    (L.kw "--or"
    `L.argText` "X Y"
    `L.text` "Logical Or.")
    $ \(B x) (B y) -> x || y

main :: IO ()
main = do
  args <- IO.getArgs
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
    Right bools -> mapM_ print bools

