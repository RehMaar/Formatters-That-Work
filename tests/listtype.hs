module A where

import Data.Foldable

a :: [Integer] -> Integer
a = foldr (+) 0
