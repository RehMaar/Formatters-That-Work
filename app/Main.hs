module Main where

import System.Environment

handle file = error "not implemented"

main :: IO ()
main =
  do
     s <- getArgs
     if null s
         then
            putStrLn "error: no input file."
         else
            handle (head s) >>= putStr
