{-# LANGUAGE TupleSections #-}

module Test where

import Data.List
import qualified Data.Map as M


main :: IO ()
main = interact go
  where
    go :: String -> String
    go = show . reverse . sortOn snd . M.toList . M.fromListWith (+) . map (, 1)