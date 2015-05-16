import System.Environment
import Text.LambdaOptions


options :: Options IO ()
options = do
    addOption (kw ["--help", "-h"] `text` "Display this help text.") $ \(HelpDescription desc) -> do
        putStrLn "Usage:"
        putStrLn desc
    addOption (kw "--user" `argText` "NAME" `text` "Prints name.") $ \name -> do
        putStrLn $ "Name:" ++ name
    addOption (kw "--user" `argText` "NAME AGE" `text` "Prints name and age.") $ \name age -> do
        putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)


main :: IO ()
main = do
    args <- getArgs
    result <- runOptions options args
    case result of
        Left (ParseFailed msg _ _) -> do
            putStrLn msg
            desc <- getHelpDescription options
            putStrLn desc
        Right action -> action


