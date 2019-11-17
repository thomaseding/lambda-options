{-# LANGUAGE Safe #-}

module Text.LambdaOptions.Example.Example_3_Repl (
  main,
) where

import qualified System.IO as IO
import qualified Text.LambdaOptions as L

data Command
  = DisplayHelp
  | Quit
  | ComputeSum [Double]
  | ComputeProduct [Double]

options :: L.Options Command ()
options = do

  L.addOption
    (L.kw "help"
    `L.text` "Display this help text.")
    $ DisplayHelp

  L.addOption
    (L.kw "quit"
    `L.text` "Quits the program.")
    $ Quit

  L.addOption
    (L.kw "sum"
    `L.argText` "NUMS*"
    `L.text` "Sums NUMS and prints the result.")
    $ ComputeSum . L.unList

  L.addOption
    (L.kw "product"
    `L.argText` "NUMS*"
    `L.text` "Multiplies NUMS and prints the result.")
    $ ComputeProduct . L.unList

main :: IO ()
main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  readEvalPrintLoop

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  c <- getCommand
  quit <- execCommand c
  putStrLn ""
  case quit of
    True  -> pure ()
    False -> readEvalPrintLoop

getCommand :: IO Command
getCommand = do
  putStr "> "
  input <- getLine
  let args = words input
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
      getCommand
    Right cs -> case cs of
      []  -> getCommand
      [c] -> pure c
      _   -> do
        putStrLn "Please enter only one command."
        putStrLn $ L.getHelpDescription options
        getCommand

execCommand :: Command -> IO Bool
execCommand c = case c of
  DisplayHelp -> do
    putStrLn $ L.getHelpDescription options
    pure False

  Quit -> pure True

  ComputeSum xs -> do
    print $ sum xs
    pure False

  ComputeProduct xs -> do
    print $ product xs
    pure False

