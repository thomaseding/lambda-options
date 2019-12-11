{-# LANGUAGE Safe #-}

-- | View my source code to see example.
module Text.LambdaOptions.Example.Example_Record (
  main,
) where

import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

data Options = Options
  { foo :: Maybe (Int, String)
  , bar :: [Double]
  , baz :: Bool
  } deriving Show

options :: L.Options (State Options ())
options = do

  -- Forces ambiguous type.
  let go :: State Options () -> State Options ()
      go = id

  L.addOption
    (L.kw "--foo"
    `L.argText` "INT STR"
    `L.text` "Do Foo.")
    $ \int str -> go $ do
      State.modify $ \st -> st { foo = Just (int, str) }

  L.addOption
    (L.kw "--bar"
    `L.argText` "NUMS*"
    `L.text` "Do Bar.")
    $ \(L.List xs) -> go $ do
      State.modify $ \st -> st { bar = xs }

  L.addOption
    (L.kw "--baz"
    `L.text` "Do Baz.")
    $ go $ do
      State.modify $ \st -> st { baz = True }

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
    Right actions -> do
      let opts = Options
            { foo = Nothing
            , bar = []
            , baz = False
            }
          opts' = State.execState (sequence_ actions) opts
      print opts'

