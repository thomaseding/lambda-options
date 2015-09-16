# lambda-options-haskell

A modern command-line parser for Haskell.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* BSD 1-Clause License

--------------

Basic example:
```haskell
import System.Environment
import Text.LambdaOptions


options :: Options IO () ()
options = do
    addOption (kw ["--help", "-h"] `text` "Display this help text.") $ do
        putStrLn "Usage:"
        getHelpDescription options >>= putStrLn
    addOption (kw "--user" `argText` "NAME" `text` "Prints name.") $ \name -> do
        putStrLn $ "Name:" ++ name
    addOption (kw "--user" `argText` "NAME AGE" `text` "Prints name and age.") $ \name age -> do
        putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)


main :: IO ()
main = do
    args <- getArgs
    case runOptions options args of
        Left (ParseFailed msg _ _) -> do
            putStrLn msg
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