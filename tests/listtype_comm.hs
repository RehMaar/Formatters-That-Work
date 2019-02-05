module A where

import Data.Foldable

a :: {- 1 -} [ {- 2 -} Integer {- 3 -} ] {- 4 -} -> Integer
a = foldr (+) 0
