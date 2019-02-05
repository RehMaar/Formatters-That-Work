module Main where

import System.Environment

import CommonASTParser
import qualified SimpleConverter as Simple
import qualified OneLineNoCommentsPrinter as Simple
import qualified AnnConverter as Ann
import qualified OneLinePrinter as Ann

handle mode file =
  do
    res <- parseAST file
    case res of
      Left str -> error str
      Right ast -> return $ switch mode ast
  where
    switch [] = Simple.oneline . Simple.toSimpl
    switch "simple" = Simple.oneline . Simple.toSimpl
    switch "comment" = Ann.oneline . Ann.toAnn

main :: IO ()
main =
  do
     s <- getArgs
     if null s
         then
            putStrLn "error: need mode (`simple` or `comment`) and file."
         else
            handle (head s) (s!!1) >>= putStr
