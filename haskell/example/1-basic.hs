import System.Environment
import Text.LambdaOptions


options :: Options IO ()
options = do
    addOption (kw "--help") $ do
        putStrLn "--user NAME [AGE]"
    addOption (kw "--user") $ \name -> do
        putStrLn $ "Name:" ++ name
    addOption (kw "--user") $ \name age -> do
        putStrLn $ "Name:" ++ name ++ " Age:" ++ show (age :: Int)


main :: IO ()
main = do
    args <- getArgs
    mError <- runOptions options args
    case mError of
        Just (ParseFailed msg _ _) -> putStrLn msg
        Nothing -> return ()



