module Types
  ( Command
  , Message
  , BoardSide(..)
  , PieceType(..)
  , Player(..)
  , Piece(..)
  , showTile
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

-- BOARDSIDE
data BoardSide
  = QueenSide
  | KingSide
  deriving (Enum, Eq, Ord, Show)

-- PIECETYPE
data PieceType
  = King
  | Queen
  | Bishop
  | Knight
  | Rook
  | Pawn
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
  deriving (Eq, Show, Enum)

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
