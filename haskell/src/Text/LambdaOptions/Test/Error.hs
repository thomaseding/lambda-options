{-# LANGUAGE Safe #-}

module Text.LambdaOptions.Test.Error (
  main,
) where

import qualified System.Exit as IO
import qualified Text.LambdaOptions as L

options :: L.Options Int
options = do

  L.addOption
    (L.kw "add")
    $ \x y ->
      x + (y :: Int)

testWithArgs :: Int -> Int -> String -> Bool
testWithArgs expectedBegin expectedEnd argsStr =
  case L.runOptions options (words argsStr) of
    Right _ -> False
    Left e -> case e of
      L.ParseFailed
        { L.parseFailedBeginArgsIndex = begin
        , L.parseFailedEndArgsIndex = end
        } ->
          (expectedBegin, expectedEnd) == (begin, end)

tests :: [Bool]
tests =
  [ testWithArgs 0 2 "add"
  , testWithArgs 0 3 "add 7"
  , testWithArgs 0 3 "add 7 True"
  , testWithArgs 0 2 "add True 7"
  ]

main :: IO ()
main = case and tests of
  True  -> IO.exitSuccess
  False -> IO.exitFailure

