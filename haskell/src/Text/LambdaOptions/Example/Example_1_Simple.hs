{-# LANGUAGE Safe #-}

-- | View my source code to see example.
module Text.LambdaOptions.Example.Example_1_Simple (
  main,
) where

import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

-- Options that get parsed into 'IO ()' actions.
options :: L.Options (IO ())
options = do

  L.addOption
    (L.kw ["--help", "-h"]
    `L.text` "Display this help text.")
    $ do
      putStrLn "Usage:"
      putStrLn $ L.getHelpDescription options

  L.addOption
    (L.kw "--add"
    `L.argText` "X Y"
    `L.text` "Adds two Doubles and prints their sum.")
    $ \x y -> do -- The arguments of the lambda imply the arguments of the option.
      -- The result of the parsed option is the sum printing IO action.
      print $ x + (y :: Double) -- (Type signature forces ambigous 'Num' type.)

-- | Try with these succeeding examples:
--
-- > :main --help
-- > :main --add 1 2
-- > :main --add 3 4 --add 5 6
-- > :main --add -1 3.14
--
-- Also try with these failing examples:
--
-- > :main --add 0
-- > :main --add 1 2 3
-- > :main --add 123
-- > :main --add True 0
-- > :main --add 0 True
main :: IO ()
main = do
  args <- Env.getArgs
  case L.runOptions options args of

    -- Any parse errors get returned in 'Left'.
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options

    -- No errors occurred during parsing.
    -- Parsed option results are returned in 'Right'.
    Right results -> do
      -- In this example, our results are 'IO ()' actions
      -- so we just sequence them to enact the results.
      sequence_ results

