module Types
  ( Command
  , Message
  , BoardSide(..)
  , PieceType(..)
  , Player(..)
  , Piece(..)
  , nextPlayer
  , squareColor
  ) where

import Data.Char (toUpper)
import Position

-- This file contains all types which are too small to deserve their own file
type Command = String

type Message = String

-- color of that tile on the board
squareColor :: Position -> Player
squareColor (x, y) =
  if even x == even y
    then Black
    else White

-- BOARDSIDE
data BoardSide
  = QueenSide
  | KingSide
  deriving (Eq, Enum)

-- PIECETYPE
data PieceType
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn
  deriving (Eq, Enum)

instance Show PieceType where
  show Pawn = "P"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show Queen = "Q"
  show King = "K"

-- PLAYER
data Player
  = Black
  | White
  deriving (Eq, Enum, Show)

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
