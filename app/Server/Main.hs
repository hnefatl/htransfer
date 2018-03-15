module Main where

import System.Environment
import Arguments

import NetUtil

main :: IO ()
main = do
        args <- getArgs
        case parseArgs args of
            Left err -> print err
            Right resources -> runServer resources