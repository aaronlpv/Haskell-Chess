module Board
  ( Board
  , pieceAt
  , allPieces
  , board0
  , applyMove
  , applyMoves
  , applyMovesAndChanges
  , clearPath
  , getKingPos
  , pieceAtOwnedBy
  ) where

import Data.Array
import Data.Maybe (fromJust, isJust, isNothing)

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

-- the association list of all pieces on the board and their positions
allPieces :: Board -> [(Position, Piece)]
allPieces b =
  map (\(pos, piece) -> (pos, fromJust piece)) $
  filter (\(_, piece) -> isJust piece) $
  map (\pos -> (pos, pieceAt b pos)) [(x, y) | y <- [0 .. 7], x <- [0 .. 7]]

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
      if y `elem` [0, 7]
        then Just (Piece c Queen)
        else p
    promote _ p = p

applyMove :: Board -> Move -> Board
applyMove b m = changeBoard b (moveToChange b m)

applyMoves :: Board -> [Move] -> Board
applyMoves b ms = changeBoard b (concatMap (moveToChange b) ms)

applyMovesAndChanges :: Board -> [Move] -> [(Position, Maybe Piece)] -> Board
applyMovesAndChanges b ms cs =
  changeBoard b (cs ++ concatMap (moveToChange b) ms)

-- checks if there is a clear path between 2 positions
clearPath :: Board -> Position -> Position -> Bool
clearPath b from to = all (isNothing . pieceAt b) (between from to)

-- gets the positions of both kings
getKingPos :: Board -> (Position, Position)
getKingPos b = loop (allPieces b) Nothing Nothing
  where
    loop ::
         [(Position, Piece)]
      -> Maybe Position
      -> Maybe Position
      -> (Position, Position)
    loop _ (Just x) (Just y) = (x, y)
    loop ((pos, Piece Black King):pcs) x _ = loop pcs x (Just pos)
    loop ((pos, Piece White King):pcs) _ y = loop pcs (Just pos) y
    loop (_:pcs) x y = loop pcs x y
    loop _ _ _ = error "Not enough kings on board"

pieceAtOwnedBy :: Board -> Player -> Position -> Bool
pieceAtOwnedBy b pl pos =
  case pieceAt b pos of
    Nothing -> False
    Just (Piece pl2 k) -> pl == pl2
