module State
  ( State(..)
  , printState
  , state0
  ) where

import Board (Board, board0, pieceAt)
import Data.String (unwords)
import Position (Position)
import Types (Player(..), showTile)
import Utils (interleave)

data State =
  State
    { board :: Board
    , turn :: Player
    , wCanCastle :: (Bool, Bool) -- Queenside, kingside
    , bCanCastle :: (Bool, Bool)
    , passant :: Maybe Position -- position of the pawn that did an opener last turn, if any
    }

printState :: State -> IO ()
printState (State b t _ _ _) = do
  let (bot, xs, ys) =
        if t == White
          then (['A' .. 'H'], [0 .. 7], [7,6 .. 0])
          else (['H','G' .. 'A'], [7,6 .. 0], [0 .. 7])
  mapM_
    (putStrLn . (\(i, s) -> show (i + 1) ++ " | " ++ s))
    (zip
       ys
       (map
          (unwords . map (showTile . pieceAt b))
          [[(x, y) | x <- xs] | y <- ys]))
  putStrLn "--+----------------"
  putStrLn $ "  | " ++ interleave bot (repeat ' ')

state0 :: State
state0 = State board0 White (True, True) (True, True) Nothing
