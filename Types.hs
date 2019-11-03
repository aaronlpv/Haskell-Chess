module Types
  ( Command
  , Message
  , Move(..)
  , BoardSide(..)
  , PieceType(..)
  , Player(..)
  , Piece(..)
  , showTile
  , readMove
  , nextPlayer
  , squareColor
  ) where

import Data.Char (toUpper)
import Position (Position)

-- This file contains all types which are too small to deserve their own file

type Command = String

type Message = String

-- color of that tile on the board
squareColor :: Position -> Player
squareColor (x, y) =
  if even x == even y
    then Black
    else White

-- MOVE
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

-- BOARDSIDE
data BoardSide
  = QueenSide
  | KingSide
  deriving (Enum, Eq, Ord, Show)

-- PIECETYPE
data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Read, Enum, Eq, Ord, Show)

kindShow :: PieceType -> String
kindShow Pawn = "p"
kindShow Rook = "r"
kindShow Knight = "n"
kindShow Bishop = "b"
kindShow Queen = "q"
kindShow King = "k"

-- PLAYER
data Player
  = Black
  | White
  deriving (Eq, Show)

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black

-- PIECE
data Piece =
  Piece
    { player :: Player
    , kind :: PieceType
    }
  deriving (Eq)

instance Show Piece where
  show (Piece Black r) = kindShow r
  show (Piece White r) = map toUpper $ kindShow r

showTile :: Maybe Piece -> String
showTile Nothing = "."
showTile (Just p) = show p
