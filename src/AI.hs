import Types
import Board
import State
import Position
import Chess
import Move

depth :: Int
depth = 3

scoreForPiece :: PieceType -> Int
scoreForPiece Pawn = 1
scoreForPiece Knight = 3
scoreForPiece Bishop = 3
scoreForPiece Rook = 5
scoreForPiece Queen = 9
scoreForPiece King = 0
