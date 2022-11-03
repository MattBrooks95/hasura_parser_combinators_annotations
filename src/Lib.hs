module Lib
    ( someFunc
    ) where

import System.Environment (
    getArgs
    )

import Data.Foldable (
    for_
    )

import MonadicParser (
    json
    , run
    )

someFunc :: IO ()
someFunc = do
    args <- getArgs
    print args
    for_ args $ \filename -> do
        content <- readFile filename
        putStrLn content
        print $ run json content
