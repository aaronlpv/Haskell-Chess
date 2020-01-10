module Board
  ( Board
  , pieceAt
  , justPieceAt
  , allPieces
  , board0
  , applyMove
  , clearPath
  , pieceAtOwnedBy
  ) where

import Data.Array
import Data.Maybe (fromJust, isJust, isNothing, maybeToList)

import Move
import Position
import Types

-- the board is a 1d array of Maybe Piece
-- arrays are immutable so unfortunately we have to copy on every change
-- but we get O(1) access which is very, very nice for Chess
type Board = Array Int (Maybe Piece)

posToIdx :: Position -> Int
posToIdx (x, y) = y * 8 + x

-- get piece at a certain position
pieceAt :: Board -> Position -> Maybe Piece
pieceAt b p = b ! posToIdx p

justPieceAt :: Board -> Position -> Piece
justPieceAt b = fromJust . pieceAt b

-- the association list of all pieces on the board and their positions
allPieces :: Board -> [(Position, Piece)]
allPieces b =
  [ ((x, y), piece)
  | x <- [0 .. 7]
  , y <- [0 .. 7]
  , piece <- maybeToList (pieceAt b (x, y))
  ]

-- the order of the pieces
order :: [PieceType]
order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

rowsOf :: Int -> Maybe Piece -> [Maybe Piece]
rowsOf n = replicate (n * 8)

board0 :: Board
board0 =
  listArray
    (0, 63)
    (map (Just . Piece White) order ++
     1 `rowsOf` Just (Piece White Pawn) ++
     4 `rowsOf` Nothing ++
     1 `rowsOf` Just (Piece Black Pawn) ++ map (Just . Piece Black) order)

-- CHANGING THE BOARD
-- applies a list of changes to the board in O(n)
changeBoard :: Board -> [(Position, Maybe Piece)] -> Board
changeBoard b = (b //) . map (\(pos, piece) -> (posToIdx pos, piece))

-- convert a move to a 'change list'
-- also promotes pawns when appropriate
moveToChange :: Board -> Move -> [(Position, Maybe Piece)]
moveToChange b (Move from to) =
  [(from, Nothing), (to, promote to (pieceAt b from))]
  where
    promote (x, y) p@(Just (Piece c Pawn)) =
      if shouldPromote to
        then Just (Piece c Queen)
        else p
    promote _ p = p

-- apply a move
-- also performs castling and en passant
applyMove :: Board -> Move -> Board
applyMove b m@(Move from@(fx, fy) to@(tx, ty))
  | kind fPiece == King =
    case isCastling m of
      Just side -> applyMoves b [castleRookMove (player fPiece) side, m]
      _ -> applyMoves b [m]
  | kind fPiece == Pawn && not (sameCol from to) && isNothing tPiece =
    applyMovesAndChanges b [m] [((tx, fy), Nothing)]
  | otherwise = applyMoves b [m]
  where
    fPiece = justPieceAt b from
    tPiece = pieceAt b to

applyMoves :: Board -> [Move] -> Board
applyMoves b ms = changeBoard b (concatMap (moveToChange b) ms)

applyMovesAndChanges :: Board -> [Move] -> [(Position, Maybe Piece)] -> Board
applyMovesAndChanges b ms cs =
  changeBoard b (cs ++ concatMap (moveToChange b) ms)

-- checks if there is a clear path between 2 positions
clearPath :: Board -> Position -> Position -> Bool
clearPath b from to = all (isNothing . pieceAt b) (between from to)

pieceAtOwnedBy :: Board -> Player -> Position -> Bool
pieceAtOwnedBy b pl pos =
  case pieceAt b pos of
    Nothing -> False
    Just (Piece pl2 k) -> pl == pl2
