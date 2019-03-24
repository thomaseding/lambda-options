# lambda-options-haskell

Declarative command line parser using type-driven pattern matching.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.

Homepage: [https://github.com/thomaseding/lambda-options](https://github.com/thomaseding/lambda-options)

Hackage: [https://hackage.haskell.org/package/lambda-options](https://hackage.haskell.org/package/lambda-options)

--------------

Basic example:
```haskell
import System.Environment
import Text.LambdaOptions


options :: Options IO () ()
options = do
    addOption (kw ["--help", "-h"] `text` "Display this help text.") $ do
        putStrLn "Usage:"
        putStrLn $ getHelpDescription options
    addOption (kw "--user" `argText` "NAME" `text` "Prints name.") $ \name -> do
        putStrLn $ "Name:" ++ name
    addOption (kw "--user" `argText` "NAME AGE" `text` "Prints name and age.") $ \name age -> do
        putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)


main :: IO ()
main = do
    args <- getArgs
    case runOptions options args of
        Left e -> do
            putStrLn $ parseFailedMessage e
            putStrLn $ getHelpDescription options
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
