module Main where

import System.Environment

import CommonASTParser
import TtgGarden
import OneLinePrinter

handle file =
  do
    res <- parseAST file
    case res of
      Left str -> error str
      Right ast -> return $ oneline $ toSimpl ast

main :: IO ()
main =
  do
     s <- getArgs
     if null s
         then
            putStrLn "error: no input file."
         else
            handle (head s) >>= putStr
