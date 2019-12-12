module Move
  ( Move(..)
  , readMove
  , pawnOpener
  , validMove
  , validMoves
  , validPawnMove
  , validCapture
  ) where

import Position
import Types
import Utils

data Move =
  Move Position Position
  deriving (Eq, Show)

readMove :: String -> Maybe Move
readMove [c1, c2, c3, c4]
  | all validChar [c1, c3] && all validDigit [c2, c4] =
    Just (Move (charToInt c1, digitToInt c2) (charToInt c3, digitToInt c4))
  | otherwise = Nothing
  where
    validChar c = c >= 'a' && c <= 'h'
    validDigit c = c >= '1' && c <= '8'
    charToInt c = fromEnum c - fromEnum 'a'
    digitToInt c = fromEnum c - fromEnum '1'
readMove _ = Nothing

-- checks if a move is only one tile away (also diagonally)
oneStep :: Move -> Bool
oneStep (Move (x1, y1) (x2, y2)) = diff x1 x2 <= 1 && diff y1 y2 <= 1

-- checks if a move could be a pawn opener
pawnOpener :: Move -> Bool
pawnOpener (Move from@(x1, y1) to@(x2, y2)) =
  sameCol from to && (y1, y2) `elem` [(1, 3), (6, 4)]

-- checks if a certain player's pawn may do a pawn opener at position
canDoOpener :: Player -> Position -> Bool
canDoOpener White (x, y) = y == 1
canDoOpener Black (x, y) = y == 6

-- checks if a move is pseudolegal for a certain piece
-- pseudolegal because we do not check the board at all, only the move itself
validMove :: Piece -> Move -> Bool
validMove (Piece Black Pawn) m@(Move (x1, y1) (x2, y2)) =
  y1 > y2 && validPawnMove m
validMove (Piece White Pawn) m@(Move (x1, y1) (x2, y2)) =
  y1 < y2 && validPawnMove m
validMove (Piece _ Bishop) (Move from to) = sameDiag from to
validMove (Piece _ Knight) (Move (x1, y1) (x2, y2)) =
  let (dx, dy) = (diff x1 x2, diff y1 y2)
   in (dx == 1 && dy == 2) || (dx == 2 && dy == 1)
validMove (Piece _ Rook) (Move from to) = sameRow from to || sameCol from to
validMove (Piece _ Queen) (Move from to) =
  sameRow from to || sameCol from to || sameDiag from to
validMove (Piece _ King) m = oneStep m

-- generates all valid moves for a certain piece
validMoves :: Piece -> Position -> [[Position]]
validMoves (Piece _ Knight) (x, y) =
  map (: []) $
  filter
    validPos
    [ (x - 1, y - 2)
    , (x - 2, y - 1)
    , (x + 2, y + 1)
    , (x + 1, y + 2)
    , (x + 2, y - 1)
    , (x - 2, y + 1)
    , (x + 1, y - 2)
    , (x - 1, y + 2)
    ]
validMoves (Piece _ King) (x, y) =
  map (: []) $
  filter
    validPos
    [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a, b) /= (0, 0)]
validMoves (Piece _ Queen) (x, y) = raysFrom (x, y)
validMoves (Piece _ Rook) (x, y) = straightRays (x, y)
validMoves (Piece _ Bishop) (x, y) = diagRays (x, y)
validMoves (Piece c@Black Pawn) p@(x, y) =
  map (: []) (filter validPos [(x + 1, y - 1), (x - 1, y - 1)]) ++ [forward]
  where
    forward =
      if canDoOpener c p
        then [(x, y - 1), (x, y - 2)]
        else filter validPos [(x, y - 1)]
validMoves (Piece c@White Pawn) p@(x, y) =
  map (: []) (filter validPos [(x + 1, y + 1), (x - 1, y + 1)]) ++ [forward]
  where
    forward =
      if canDoOpener c p
        then [(x, y + 1), (x, y + 2)]
        else filter validPos [(x, y + 1)]

validPawnMove :: Move -> Bool
validPawnMove m = oneStep m || pawnOpener m

-- checks if a capture move is valid (only matters for pawns)
validCapture :: Piece -> Move -> Bool
validCapture p@(Piece _ Pawn) m@(Move (x1, y1) (x2, y2)) =
  validMove p m && x1 /= x2
validCapture p m = validMove p m
