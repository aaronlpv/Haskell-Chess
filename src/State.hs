module State
  ( State(..)
  , state0
  ) where

import Board
import Position
import Types

data State =
  State
    { board :: Board
    , turn :: Player
    , wCanCastle :: (Bool, Bool) -- Queenside, kingside
    , bCanCastle :: (Bool, Bool)
    , passant :: Maybe Position -- position of the pawn that did an opener last turn, if any
    }

state0 :: State
state0 = State board0 White (True, True) (True, True) Nothing
