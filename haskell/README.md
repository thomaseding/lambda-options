# lambda-options-haskell

Declarative command line parser using type-driven pattern matching.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.

Homepage: [https://github.com/thomaseding/lambda-options](https://github.com/thomaseding/lambda-options)

Hackage: [https://hackage.haskell.org/package/lambda-options](https://hackage.haskell.org/package/lambda-options)

--------------

Basic example:
```haskell
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
```

```
$ example.exe --user HaskellCurry 81 --user GraceHopper
Name:HaskellCurry Age:81
Name:GraceHopper
$ example.exe -h
Usage:
 -h, --help                  Display this help text.
     --user NAME             Prints name.
     --user NAME AGE         Prints name and age.
$ example.exe --user Pythagoras LXXV
Unknown option at index 2: `LXXV'
Usage:
 -h, --help                  Display this help text.
     --user NAME             Prints name.
     --user NAME AGE         Prints name and age.
```
