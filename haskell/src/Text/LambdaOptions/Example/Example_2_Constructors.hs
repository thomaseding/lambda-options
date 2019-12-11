{-# LANGUAGE Safe #-}

-- | View my source code to see example.
module Text.LambdaOptions.Example.Example_2_Constructors (
  main,
) where

import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

data Option
  = Foo Int String
  | Bar [Double]
  | Baz
  deriving Show

options :: L.Options Option
options = do

  L.addOption
    (L.kw "--foo"
    `L.argText` "INT STR"
    `L.text` "Do Foo.")
    $ Foo

  L.addOption
    (L.kw "--bar"
    `L.argText` "NUMS*"
    `L.text` "Do Bar.")
    $ Bar . L.unList

  L.addOption
    (L.kw "--baz"
    `L.text` "Do Baz.")
    $ Baz

-- | Try with these succeeding examples:
--
-- > :main --foo 1 abc
-- > :main --bar
-- > :main --bar 4 5 6
-- > :main --baz
-- > :main --foo 1 abc --bar 4 5 6 --baz
-- > :main --baz --bar 4 5 6 --foo 1 abc
--
-- Also try with these failing examples:
--
-- > :main --foo abc
-- > :main --foo 1.5 abc
-- > :main --bar a b c
main :: IO ()
main = do
  args <- Env.getArgs
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
    Right opts -> print opts

