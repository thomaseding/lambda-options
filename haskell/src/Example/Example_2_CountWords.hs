{-# LANGUAGE Safe #-}

module Example.Example_2_CountWords where


import System.Environment
import Text.LambdaOptions


options :: Options (Int -> Int) ()
options = do
    addOption (kw ()) (+ (1 :: Int))


main :: IO ()
main = do
    args <- getArgs
    case runOptions options args of
        Left {} -> error "Internal logic error."
        Right fs -> print $ foldr (.) id fs 0


