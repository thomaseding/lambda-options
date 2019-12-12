# lambda-options-haskell

Declarative command line parser using type-driven pattern matching.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.

Homepage: [https://github.com/thomaseding/lambda-options](https://github.com/thomaseding/lambda-options)

Hackage: [https://hackage.haskell.org/package/lambda-options](https://hackage.haskell.org/package/lambda-options)

--------------

Basic example:
```haskell
import qualified System.Environment as Env
import qualified Text.LambdaOptions as L

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
    $ \x y -> do
      print $ x + (y :: Double)

main :: IO ()
main = do
  args <- Env.getArgs
  case L.runOptions options args of
    Left e -> do
      putStrLn $ L.prettyOptionsError e
      putStrLn $ L.getHelpDescription options
    Right results -> do
      sequence_ results
```

```
>>> :main --add 3 0.14
3.14
>>> :main -h
Usage:
     --add X Y               Adds two Doubles and prints their sum.
 -h, --help                  Display this help text.
>>> :main --add 0 1 --add 2 four
Bad input for `--add' at index 3: `four'
     --add X Y               Adds two Doubles and prints their sum.
 -h, --help                  Display this help text.
```
