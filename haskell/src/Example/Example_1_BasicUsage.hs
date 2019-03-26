{-# LANGUAGE Safe #-}

module Example.Example_1_BasicUsage where


import System.Environment
import Text.LambdaOptions


options :: Options (IO ()) ()
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


