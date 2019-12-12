{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- | View my source code to see example.
module Text.LambdaOptions.Example.Example_4_Booly (
  main,
) where

import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

type B = L.Booly
  'L.AllowWord
  'L.DisallowLetter
  'L.AllowBit
  ('L.OrCasing 'L.LowerAll 'L.UpperAll)

pattern B :: Bool -> B
pattern B x = L.Booly x

options :: L.Options Bool
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

-- | Try with these succeeding examples:
--
-- > :main --and true false
-- > :main --and TRUE FALSE
-- > :main --and 1 0
-- > :main --and true 0 --or 1 TRUE
--
-- Also try with these failing examples:
--
-- > :main --and true
-- > :main --and t f
-- > :main --and 0 2
-- > :main --and True False
main :: IO ()
main = do
  args <- Env.getArgs
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
    Right bools -> mapM_ print bools

