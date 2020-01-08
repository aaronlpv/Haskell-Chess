module Types
  ( Command
  , Message
  , BoardSide(..)
  , PieceType(..)
  , Player(..)
  , Piece(..)
  , nextPlayer
  ) where

import Position

-- This file contains all types which are too small to deserve their own file
type Command = String

type Message = String

-- BOARDSIDE
data BoardSide
  = QueenSide
  | KingSide
  deriving (Show, Eq, Enum)

-- PIECETYPE
data PieceType
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn
  deriving (Eq, Enum, Read)

instance Show PieceType where
  show Pawn = ""
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
  deriving (Show, Eq)
