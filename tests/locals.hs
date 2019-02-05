f = ap
  where
      bp :: Int -> Int
      ap :: Int -> Int
      ap n = bp n + cp n

      bp n = n + 10

      cp :: Int -> Int
      cp n = n * 10
