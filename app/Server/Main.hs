module Main where

import System.Environment
import Control.Monad.Catch

import Arguments
import Server

main :: IO ()
main = handleAll (print . displayException) $ do
        args <- getArgs
        resources <- parseArgs args
        runServer "0.0.0.0" "34652" resources