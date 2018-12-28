module Main where

import System.Environment

import TestParser

main :: IO ()
main =
  do
     s <- getArgs
     if null s
         then
            putStrLn "error: no input file."
         else
            oneline (head s) >>= putStr
