module Utils
  ( diff
  , classicCompare
  ) where

-- absolute difference between 2 ints
diff :: Int -> Int -> Int
diff x y = abs (x - y)

-- C style compare
classicCompare :: Int -> Int -> Int
classicCompare y x = fromEnum (compare y x) - 1
