module Position
  ( Position
  , sameRow
  , sameCol
  , sameDiag
  , validPos
  , isBehind
  , between
  , ray
  , straightRays
  , diagRays
  , raysFrom
  ) where

import Utils (classicCompare, diff)

type Position = (Int, Int)

sameRow :: Position -> Position -> Bool
sameRow (x1, y1) (x2, y2) = y1 == y2

sameCol :: Position -> Position -> Bool
sameCol (x1, y1) (x2, y2) = x1 == x2

sameDiag :: Position -> Position -> Bool
sameDiag (x1, y1) (x2, y2) = diff x1 x2 == diff y1 y2

validPos :: Position -> Bool
validPos (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

-- checks if a positions is 'behind' another position
-- behind means same column and 1 closer to the closest 'player side'
-- this is used to check for en passant
isBehind :: Position -> Position -> Bool
isBehind p1@(x1, y1) p2@(x2, y2)
  | not (sameCol p1 p2) = False
  | y2 <= 3 = y1 == y2 - 1
  | otherwise = y1 == y2 + 1

-- generates a list of positions between 2 positions
between :: Position -> Position -> [Position]
between from@(x1, y1) to@(x2, y2) = take num (ray from (dx, dy))
  where
    num = max (diff x1 x2) (diff y1 y2) - 1
    dx = classicCompare x2 x1
    dy = classicCompare y2 y1

-- generates a list of positions in a certain direction
ray :: Position -> (Int, Int) -> [Position]
ray (x, y) (dx, dy) =
  takeWhile validPos [(x + a * dx, y + a * dy) | a <- [1 ..]]

straightRays :: Position -> [[Position]]
straightRays pos = map (ray pos) [(0, 1), (1, 0), (0, -1), (-1, 0)]

diagRays :: Position -> [[Position]]
diagRays pos = map (ray pos) [(1, 1), (-1, -1), (1, -1), (-1, 1)]

-- straight and diagonal rays 'radiating' outwards from a position
raysFrom :: Position -> [[Position]]
raysFrom pos = straightRays pos ++ diagRays pos
