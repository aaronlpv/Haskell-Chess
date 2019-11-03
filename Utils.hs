module Utils
  ( interleave
  , diff
  , classicCompare
  ) where

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- absolute difference between 2 ints
diff :: Int -> Int -> Int
diff x y = abs (x - y)

-- C style compare
classicCompare :: Int -> Int -> Int
classicCompare y x = fromEnum (compare y x) - 1