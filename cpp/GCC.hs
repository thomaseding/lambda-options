module GCC where


import System.Environment
import System.Process


main :: IO ()
main = do
    args <- getArgs
    let allArgs = [
            "-std=c++11",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-pedantic",
            "-fstrict-aliasing",
            "-g" ]
            ++ args
    rawSystem "g++" allArgs
    return ()


