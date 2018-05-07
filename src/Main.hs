module Main where

import Development.Shake

main :: IO ()
{-
main = do
  putStrLn "hello world"
main :: IO ()
-}

main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["help1"]

    phony "help1" $ do
        putNormal "build1"

    phony "help2" $ do
        putNormal "build2"
