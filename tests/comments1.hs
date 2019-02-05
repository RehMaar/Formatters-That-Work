---- FIRST ----- ___
{-# LANGUAGE GADTs #-}

module {- 1 -} CommentTest {- 2 -} where {- 3 -}

import Data.Foldable as DF

-- Test Again
f {- How to -} n {- HA! -} = {- save place -} n {- EVERYEVERE -} + {- where comments are -} 10 -- Who knows!

g n = n + k
  where {- Hehe! -} k = {- 10? NO! -} 20
