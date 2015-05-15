import System.Environment
import System.Exit
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




