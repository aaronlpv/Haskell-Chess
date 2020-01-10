module Chess
  ( state0
  , stateDoMove
  , legalMoves
  , KingInfo(..)
  , State(..)
  ) where

import Data.Maybe (fromJust, isJust, isNothing, maybeToList)

import Board
import Move
import Position
import Types
import Utils

data KingInfo =
  KingInfo
    { kingPos :: Position
    , mayCastleQ :: Bool
    , mayCastleK :: Bool
    }

data State =
  State
    { board :: Board
    , turn :: Player
    , isChecked :: Bool
    , whiteKing :: KingInfo
    , blackKing :: KingInfo
    , enPassant :: Maybe Int
    }

state0 :: State
state0 =
  State
    board0
    White
    False
    (KingInfo (4, 0) True True)
    (KingInfo (4, 7) True True)
    Nothing

stateKingInfo :: Player -> State -> KingInfo
stateKingInfo p =
  if p == Black
    then blackKing
    else whiteKing

stateKingPos :: Player -> State -> Position
stateKingPos p s = kingPos $ stateKingInfo p s

kinfoMayCastle :: BoardSide -> KingInfo -> Bool
kinfoMayCastle bs =
  if bs == QueenSide
    then mayCastleQ
    else mayCastleK

updateKingInfo :: State -> Move -> (KingInfo, KingInfo)
updateKingInfo s (Move from to) =
  if kind piece == King
    then if player piece == White
           then (KingInfo to False False, updateCastlePerms bKing Black)
           else (updateCastlePerms wKing White, KingInfo to False False)
    else (updateCastlePerms wKing White, updateCastlePerms bKing Black)
  where
    piece = justPieceAt (board s) from
    wKing = whiteKing s
    bKing = blackKing s
    doesNotAffect pos = pos `notElem` [from, to]
    updateCastlePerms (KingInfo p q k) White =
      KingInfo p (q && doesNotAffect (0, 0)) (k && doesNotAffect (7, 0))
    updateCastlePerms (KingInfo p q k) Black =
      KingInfo p (q && doesNotAffect (0, 7)) (k && doesNotAffect (7, 7))

filterMoves :: Board -> Position -> [[Position]] -> [[Position]]
filterMoves b p = filter (not . null) . map f
  where
    play = player (justPieceAt b p)
    f (m:ms) =
      case pieceAt b m of
        Nothing -> m : f ms
        Just (Piece pl _) ->
          if play == pl
            then []
            else [m]
    f [] = []

-- checks if a player is allowed to castle to a certain side
castlingAllowed :: State -> BoardSide -> Bool
castlingAllowed state bs =
  kinfoMayCastle bs (stateKingInfo player state) &&
  clearPath (board state) (px1, y) (px2, y) &&
  not (any (isThreatened (board state) player) (between (bx1, y) (bx2, y)))
  where
    player = turn state
    y =
      if player == White
        then 0
        else 7
    (px1, px2, bx1, bx2) =
      if bs == QueenSide
        then (0, 4, 1, 5)
        else (4, 7, 3, 7)

filterLegalMoves s = filter (not . leavesInCheck s)

legalMovesForPiece :: State -> (Position, Piece) -> [Move]
legalMovesForPiece s (pos, piece@(Piece p Pawn)) =
  filterLegalMoves s $
  map (Move pos) $ concatMap (takeWhile legalPawnMove) (validMoves piece pos)
  where
    legalPawnMove to =
      if sameCol pos to
        then isNothing (pieceAt (board s) to)
        else pieceAtOwnedBy (board s) (nextPlayer p) to
legalMovesForPiece s (pos, piece@(Piece p k)) = filterLegalMoves s moves
  where
    moves =
      map (Move pos) $ concat $ filterMoves (board s) pos $ validMoves piece pos

legalMoves :: State -> [Move]
legalMoves s =
  castling ++
  passants ++
  concatMap
    (legalMovesForPiece s)
    (filter (\(pos, Piece p k) -> p == turn s) (allPieces (board s)))
  where
    castling =
      [ castleMove (turn s) side
      | side <- [QueenSide, KingSide]
      , castlingAllowed s side
      ]
    (fromy, toy) =
      if turn s == White
        then (4, 5)
        else (3, 2)
    passants =
      [ Move from to
      | epx <- maybeToList (enPassant s)
      , x <- [epx - 1, epx + 1]
      , x <= 7 && x >= 0
      , let (from, to) = ((x, fromy), (epx, toy))
      , case pieceAt (board s) from of
          Nothing -> False
          Just (Piece p k) -> k == Pawn && p == turn s
      , not (leavesInCheck s (Move from to))
      ]

leavesInCheck :: State -> Move -> Bool
leavesInCheck s m@(Move from to) =
  isThreatened
    (applyMove (board s) m)
    (turn s)
    (if kind (justPieceAt (board s) from) == King
       then to
       else stateKingPos (turn s) s)

-- is a certain tile threatened by pl's opponent
-- does not take en passant into account
isThreatened :: Board -> Player -> Position -> Bool
isThreatened b pl pos =
  any dangerous $
  map (map (\apos -> (apos, b `pieceAt` apos))) (captureRays pos)
  where
    dangerous [] = False
    dangerous ((_, Nothing):xs) = dangerous xs
    dangerous ((ppos, Just p@(Piece pl2 knd)):xs) =
      pl /= pl2 && validCapture p (Move ppos pos)

stateDoMove :: State -> Move -> State
stateDoMove s@(State b t ch w bl h) m@(Move from to) =
  State
    newBoard
    newPlayer
    check
    wKing
    bKing
    (if pawnOpener m && kind (justPieceAt b from) == Pawn
       then Just (fst from)
       else Nothing)
  where
    (wKing, bKing) = updateKingInfo s m
    newBoard = applyMove b m
    newPlayer = nextPlayer t
    check =
      isThreatened
        newBoard
        newPlayer
        (kingPos
           (if newPlayer == White
              then wKing
              else bKing))
