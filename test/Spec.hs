module Main (main) where

import Bundles (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
