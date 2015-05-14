# lambda-options-haskell

A modern command-line parser for Haskell.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* BSD 1-Clause License

--------------

Basic example:
```haskell
import System.Environment
import System.ExitCode
import Text.LambdaOptions

options :: Options IO ()
options = do
  addOption "--help" $ do
    putStrLn "--user NAME [AGE]"
  addOption "--user" $ \name -> do
    putStrLn $ "Name:" ++ name
  addOption "--user" $ \name age -> do
    putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)

main :: IO ()
main = do
  args <- getArgs
  mError <- runOptions options args
  case mError of
    Just OptionsError -> exitFailure
    Nothing -> exitSuccess

```
