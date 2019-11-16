{-# LANGUAGE Safe #-}

module Example.Example_1_BasicUsage (
  main,
) where

import qualified System.Environment as IO
import qualified Text.LambdaOptions as L

options :: L.Options (IO ()) ()
options = do

  L.addOption
    (L.kw ["--help", "-h"]
    `L.text` "Display this help text.")
    $ do
      putStrLn "Usage:"
      putStrLn $ L.getHelpDescription options

  L.addOption
    (L.kw "--user"
    `L.argText` "NAME"
    `L.text` "Prints name.")
    $ \name -> do
      putStrLn $ "Name:" ++ name

  L.addOption
    (L.kw "--user"
    `L.argText` "NAME AGE"
    `L.text` "Prints name and age.")
    $ \name age -> do
      putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)

main :: IO ()
main = do
  args <- IO.getArgs
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
    Right actions -> sequence_ actions

