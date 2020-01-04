module Chess where
import Data.Maybe (fromJust, isJust, isNothing)

import Board
import Move
import Position
import State
import Types
import Utils


-- CASTLING LOGIC

-- checks if a player is allowed to castle to a certain side
-- castlingAllowed :: State -> Player -> BoardSide -> Bool
-- castlingAllowed (State b t wc (qs, ks) _) p@Black QueenSide =
--   qs &&
--   clearPath b (0, 7) (4, 7) &&
--   not (any (underAttack b p) (between (1, 7) (5, 7)))
-- castlingAllowed (State b t wc (qs, ks) _) p@Black KingSide =
--   ks &&
--   clearPath b (4, 7) (7, 7) &&
--   not (any (underAttack b p) (between (3, 7) (7, 7)))
-- castlingAllowed (State b t (qs, ks) bc _) p@White QueenSide =
--   qs &&
--   clearPath b (0, 0) (4, 0) &&
--   not (any (underAttack b p) (between (1, 0) (5, 0)))
-- castlingAllowed (State b t (qs, ks) bc _) p@White KingSide =
--   ks &&
--   clearPath b (4, 0) (4, 0) &&
--   not (any (underAttack b p) (between (1, 0) (5, 0)))


-- executes a pawn move
-- doPawnMove :: State -> Move -> Piece -> Maybe Piece -> (Message, Maybe State)
-- doPawnMove s@(State b t wc bc p) m@(Move from to) frompiece topiece
--   | pawnOpener m && isNothing topiece && clearPath b from to =
--     ("", updateState s (applyMove b m) (Just to) m)
--   | validCapture frompiece m &&
--       case p of
--         Nothing -> False
--         Just passpos -> to `isBehind` passpos =
--     ( "Just passing by"
--     , updateState
--         s
--         (applyMovesAndChanges b [m] [(fromJust p, Nothing)])
--         Nothing
--         m)
--   | otherwise = doMove s m frompiece topiece
