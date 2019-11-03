import Board
  ( Board
  , allPieces
  , applyMove
  , applyMoves
  , applyMovesAndChanges
  , board0
  , clearPath
  , getKingPos
  , pieceAt
  )
import Data.Maybe (fromJust, isJust, isNothing)
import Position
  ( Position
  , between
  , diagRays
  , isBehind
  , raysFrom
  , sameCol
  , sameDiag
  , sameRow
  , straightRays
  , validPos
  )
import State (State(..), printState, state0)
import Types
  ( BoardSide(..)
  , Command
  , Message
  , Move(..)
  , Piece(..)
  , PieceType(..)
  , Player(..)
  , nextPlayer
  , readMove
  , showTile
  , squareColor
  )
import Utils (diff)

-- checks if a move is only one tile away (also diagonally)
oneStep :: Move -> Bool
oneStep (Move (x1, y1) (x2, y2)) = diff x1 x2 <= 1 && diff y1 y2 <= 1

-- checks if a move could be a pawn opener
pawnOpener :: Move -> Bool
pawnOpener (Move from@(x1, y1) to@(x2, y2)) =
  sameCol from to && (y1, y2) `elem` [(1, 3), (6, 4)]

-- checks if a certain player's pawn may do a pawn opener at position
canDoOpener :: Player -> Position -> Bool
canDoOpener White (x, y) = y == 1
canDoOpener Black (x, y) = y == 6

validPawnMove :: Move -> Bool
validPawnMove m = oneStep m || pawnOpener m

-- checks if a move is pseudolegal for a certain piece
-- pseudolegal because we do not check the board at all, only the move itself
validMove :: Piece -> Move -> Bool
validMove (Piece Black Pawn) m@(Move (x1, y1) (x2, y2)) =
  y1 > y2 && validPawnMove m
validMove (Piece White Pawn) m@(Move (x1, y1) (x2, y2)) =
  y1 < y2 && validPawnMove m
validMove (Piece _ Bishop) (Move from to) = sameDiag from to
validMove (Piece _ Knight) (Move (x1, y1) (x2, y2)) =
  let (dx, dy) = (diff x1 x2, diff y1 y2)
   in (dx == 1 && dy == 2) || (dx == 2 && dy == 1)
validMove (Piece _ Rook) (Move from to) = sameRow from to || sameCol from to
validMove (Piece _ Queen) (Move from to) =
  sameRow from to || sameCol from to || sameDiag from to
validMove (Piece _ King) m = oneStep m

-- generates all valid moves for a certain piece
validMoves :: Piece -> Position -> [[Position]]
validMoves (Piece _ Knight) (x, y) =
  map (: []) $
  filter
    validPos
    [ (x - 1, y - 2)
    , (x - 2, y - 1)
    , (x + 2, y + 1)
    , (x + 1, y + 2)
    , (x + 2, y - 1)
    , (x - 2, y + 1)
    , (x + 1, y - 2)
    , (x - 1, y + 2)
    ]
validMoves (Piece _ King) (x, y) =
  map (: []) $
  filter
    validPos
    [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a, b) /= (0, 0)]
validMoves (Piece _ Queen) (x, y) = raysFrom (x, y)
validMoves (Piece _ Rook) (x, y) = straightRays (x, y)
validMoves (Piece _ Bishop) (x, y) = diagRays (x, y)
validMoves (Piece c@Black Pawn) p@(x, y) =
  map (: []) (filter validPos [(x + 1, y - 1), (x - 1, y - 1)]) ++ [forward]
  where
    forward =
      if canDoOpener c p
        then [(x, y - 1), (x, y - 2)]
        else filter validPos [(x, y - 1)]
validMoves (Piece c@White Pawn) p@(x, y) =
  map (: []) (filter validPos [(x + 1, y + 1), (x - 1, y + 1)]) ++ [forward]
  where
    forward =
      if canDoOpener c p
        then [(x, y + 1), (x, y + 2)]
        else filter validPos [(x, y + 1)]

-- checks if a capture move is valid (only matters for pawns)
validCapture :: Piece -> Move -> Bool
validCapture p@(Piece _ Pawn) m@(Move (x1, y1) (x2, y2)) =
  validMove p m && x1 /= x2
validCapture p m = validMove p m

-- what positions have to be looked at to see if a king is in check
-- contains lists of 'rays' as only the first piece encountered on each ray matters
captureRays :: Position -> [[Position]]
captureRays from = raysFrom from ++ validMoves (Piece White Knight) from

-- is a certain tile threatened by pl's opponent
underAttack :: Board -> Player -> Position -> Bool
underAttack b pl pos =
  any dangerous $
  map (map (\apos -> (apos, b `pieceAt` apos))) (captureRays pos)
  where
    dangerous [] = False
    dangerous ((_, Nothing):xs) = dangerous xs
    dangerous ((ppos, Just p@(Piece pl2 knd)):xs) =
      pl /= pl2 && validCapture p (Move ppos pos)

-- check if a move is a valid castle move, returns the side if it is
isCastling :: Move -> Maybe BoardSide
isCastling (Move (4, 7) (2, 7)) = Just QueenSide
isCastling (Move (4, 7) (6, 7)) = Just KingSide
isCastling (Move (4, 0) (2, 0)) = Just QueenSide
isCastling (Move (4, 0) (6, 0)) = Just KingSide
isCastling _ = Nothing

-- the move the rook has to make in every castling situation
castleRookMove :: Player -> BoardSide -> Move
castleRookMove Black QueenSide = Move (0, 7) (3, 7)
castleRookMove Black KingSide = Move (7, 7) (5, 7)
castleRookMove White QueenSide = Move (0, 0) (3, 0)
castleRookMove White KingSide = Move (7, 0) (5, 0)

-- checks if a player is allowed to castle to a certain side
castlingAllowed :: State -> Player -> BoardSide -> Bool
castlingAllowed (State b t wc (qs, ks) _) p@Black QueenSide =
  qs &&
  clearPath b (0, 7) (4, 7) &&
  not (any (underAttack b p) (between (1, 7) (5, 7)))
castlingAllowed (State b t wc (qs, ks) _) p@Black KingSide =
  ks &&
  clearPath b (4, 7) (7, 7) &&
  not (any (underAttack b p) (between (3, 7) (7, 7)))
castlingAllowed (State b t (qs, ks) bc _) p@White QueenSide =
  qs &&
  clearPath b (0, 0) (4, 0) &&
  not (any (underAttack b p) (between (1, 0) (5, 0)))
castlingAllowed (State b t (qs, ks) bc _) p@White KingSide =
  ks &&
  clearPath b (4, 0) (4, 0) &&
  not (any (underAttack b p) (between (1, 0) (5, 0)))

-- updates whether or not the player can still castle after this move
wCastlePerms :: (Bool, Bool) -> Move -> (Bool, Bool)
wCastlePerms (qc, kc) (Move (4, 0) to) = (False, False)
wCastlePerms (qc, kc) (Move from to) =
  (qc && (0, 0) `elem` [from, to], kc && (7, 0) `elem` [from, to])

bCastlePerms :: (Bool, Bool) -> Move -> (Bool, Bool)
bCastlePerms (qc, kc) (Move (4, 7) to) = (False, False)
bCastlePerms (qc, kc) (Move from to) =
  (qc && (0, 7) `elem` [from, to], kc && (7, 7) `elem` [from, to])

-- executes a castling move
doCastling :: State -> Move -> (Message, Maybe State)
doCastling s@(State b t wc bc p) m@(Move from to) =
  case isCastling m of
    Nothing -> error "Invalid castling move in doCastling"
    Just side ->
      if castlingAllowed s t side
        then ( "Castled!"
             , updateState s (applyMoves b [m, castleRookMove t side]) Nothing m)
        else ("You may not castle at this time", Just s)

-- executes a pawn move
doPawnMove :: State -> Move -> Piece -> Maybe Piece -> (Message, Maybe State)
doPawnMove s@(State b t wc bc p) m@(Move from to) frompiece topiece
  | pawnOpener m && isNothing topiece && clearPath b from to =
    ("", updateState s (applyMove b m) (Just to) m)
  | validCapture frompiece m &&
      case p of
        Nothing -> False
        Just passpos -> to `isBehind` passpos =
    ( "Just passing by"
    , updateState
        s
        (applyMovesAndChanges b [m] [(fromJust p, Nothing)])
        Nothing
        m)
  | otherwise = doMove s m frompiece topiece

-- executes a generic move
doMove :: State -> Move -> Piece -> Maybe Piece -> (Message, Maybe State)
doMove s@(State b t wc bc p) m@(Move from to) fromp@(Piece frp frk) topiece =
  if (frk == Knight || clearPath b from to) &&
     case topiece of
       Nothing -> True
       Just (Piece top _) -> top /= t
    then ("", updateState s (applyMove b m) Nothing m)
    else ("Path blocked", Just s)

step :: State -> Command -> (Message, Maybe State)
step s c =
  case readMove c of
    Just m@(Move from to) ->
      endgameCheck
        s
        (update s m (pieceAt (board s) from) (pieceAt (board s) to))
    _ -> ("Invalid command", Just s)

-- the 'main' update function
update :: State -> Move -> Maybe Piece -> Maybe Piece -> (Message, Maybe State)
update s m Nothing topiece = ("No piece there", Just s)
update s@(State b t wc bc p) m@(Move from to) (Just fromp@(Piece frp frk)) topiece
  | frp /= t = ("Not your piece", Just s)
  | frk == King && isJust (isCastling m) = doCastling s m
  | not (validMove fromp m) = ("Invalid " ++ show frk ++ " move", Just s)
  | frk == Pawn = doPawnMove s m fromp topiece
  | otherwise = doMove s m fromp topiece

-- helper function to update State
updateState :: State -> Board -> Maybe Position -> Move -> Maybe State
updateState s@(State b t wc bc p) newboard newpassants move =
  Just $
  State
    newboard
    (nextPlayer t)
    (wCastlePerms wc move)
    (bCastlePerms bc move)
    newpassants

-- checks for draw by insufficient material
-- quite the ugly function, but also creates almost no data structures
insufMaterial :: Board -> Bool
insufMaterial b = loop (allPieces b) False False False False 0 0
  where
    loop ((_, Piece Black Knight):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      loop pcs wbBshp wwBshp bbBshp bwBshp (bKnghts + 1) wKnghts
    loop ((_, Piece White Knight):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      loop pcs wbBshp wwBshp bbBshp bwBshp bKnghts (wKnghts + 1)
    loop ((pos, Piece White Bishop):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      if squareColor pos == White
        then loop pcs wbBshp True bbBshp bwBshp bKnghts wKnghts
        else loop pcs True wwBshp bbBshp bwBshp bKnghts wKnghts
    loop ((pos, Piece Black Bishop):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      if squareColor pos == White
        then loop pcs wbBshp wwBshp bbBshp True bKnghts wKnghts
        else loop pcs wbBshp wwBshp True bwBshp bKnghts wKnghts
    loop ((_, Piece _ King):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      loop pcs wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts
    loop (_:pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts = False
    loop [] wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      check wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts
    check False False False False 0 0 = True -- Only kings remain
    check False False False False 1 0 = True -- White king vs black king and knight
    check False False False False 0 1 = True -- Black king vs white king and knight
    check True False True False 0 0 = True -- Opposing bishops on black squares
    check False True False True 0 0 = True -- Opposing bishops on white squares
    check wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      sum (map fromEnum [wbBshp, wwBshp, bbBshp, bwBshp]) == 1 -- King vs King and bishop

-- checks if a certain piece has a valid (AND LEGAL!) move
hasLegalMove :: Board -> Position -> (Position, Piece) -> Bool
hasLegalMove b kingPos (pos, pc@(Piece c k)) = any check psdlegals
  where
    psdlegals = validMoves pc pos
    check [] = False
    check (to:ms) =
      case pieceAt b to of
        Just (Piece toc _) ->
          c /= toc &&
           (k /= Pawn && not (sameCol pos to)) && validState (Move pos to) 
        Nothing ->
          ((k /= Pawn || sameCol pos to) && validState (Move pos to)) ||
           check ms
      where
        validState m =
          not
            (underAttack
               (applyMove b m)
               c
               (if k == King -- if we moved the king, check for checkmate at new position
                  then to
                  else kingPos))

-- checks if a certain player has a valid and legal move
hasLegalMoves :: State -> Position -> Bool
hasLegalMoves s@(State b t wc bc p) kingPos =
  any (hasLegalMove b kingPos) pieces
  where
    pieces = filter (\(pos, Piece own _) -> own == t) (allPieces b)

-- checks for all possible game endings
endgameCheck :: State -> (Message, Maybe State) -> (Message, Maybe State)
endgameCheck old (msg, Nothing) = (msg, Just old)
endgameCheck old@(State _ ot _ _ _) (msg, Just new@(State b nt wc bc p))
  | insufMaterial b = ("Draw by insufficient material", Nothing)
  | playerChecked = ("You may not leave your king in check", Just old)
  | not
     (hasLegalMoves
        new
        (if nt == Black
           then blackKingPos
           else whiteKingPos)) =
    if opponentChecked
      then ("Checkmate! " ++ show ot ++ " wins!", Nothing)
      else ("Draw!", Nothing)
  | otherwise =
    ( if opponentChecked
        then "Check!"
        else msg
    , Just new)
  where
    (whiteKingPos, blackKingPos) = getKingPos b
    whiteChecked = underAttack b White whiteKingPos
    blackChecked = underAttack b Black blackKingPos
    playerChecked =
      (ot == White && whiteChecked) || (ot == Black && blackChecked)
    opponentChecked =
      (nt == White && whiteChecked) || (nt == Black && blackChecked)

main :: IO ()
main = loop $ Just state0
  where
    loop Nothing = return ()
    loop (Just s) = do
      printState s
      c <- getLine
      let (m, ms) = step s c
      putStrLn m
      loop ms
