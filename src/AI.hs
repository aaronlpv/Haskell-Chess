module AI(AITree(..), State(..), state0, stateDoMove, legalMoves, stateKingPos, doAI, findByMove, scoreTree, bestForPlayer, isChecked, appendHistory) where
import Types
import Board
import Position
import Chess
import Move

import Data.Maybe (fromJust, isNothing, isJust, maybeToList)
import Data.List (find, intercalate, delete)
import Control.Parallel (par, pseq)

data AITree = AINode {
  aiState :: State,
  aiMove :: Move,
  aiSucc :: [AITree] }

instance Show AITree where
  show (AINode s m succ) = "[NODE " ++ show (turn s) ++ show m  ++ "]"

data KingInfo = KingInfo { kingPos :: Position
                         , mayCastleQ :: Bool
                         , mayCastleK :: Bool
                         }

data State = State { board :: Board
                   , turn :: Player
                   , whiteKing :: KingInfo
                   , blackKing :: KingInfo
                   , enPassant :: Maybe Int
                   }

state0 :: State
state0 = State board0 White (KingInfo (4,0) True True) (KingInfo (4,7) True True) Nothing

stateKingInfo :: Player -> State -> KingInfo
stateKingInfo p = if p == Black then blackKing else whiteKing

stateKingPos :: Player -> State -> Position
stateKingPos p s = kingPos $ stateKingInfo p s

kinfoMayCastle :: BoardSide -> KingInfo -> Bool
kinfoMayCastle bs = if bs == QueenSide then mayCastleQ else mayCastleK

updateKingInfo :: State -> Move -> (KingInfo, KingInfo)
updateKingInfo s (Move from to) =
  if kind piece == King then
    if player piece == White then
      (KingInfo to False False, updateCastlePerms bKing Black)
      else
      (updateCastlePerms wKing White, KingInfo to False False)
  else (updateCastlePerms wKing White, updateCastlePerms bKing Black)
  where piece = justPieceAt (board s) from
        wKing = whiteKing s
        bKing = blackKing s
        doesNotAffect pos = pos `notElem` [from, to]
        updateCastlePerms (KingInfo p q k) White =
          KingInfo p (q && doesNotAffect (0,0)) (k && doesNotAffect (7,0))
        updateCastlePerms (KingInfo p q k) Black =
          KingInfo p (q && doesNotAffect (0,7)) (k && doesNotAffect (7,7))

filterMoves :: Board -> Position -> [[Position]] -> [[Position]]
filterMoves b p = filter (not . null) . map f
  where play = player (justPieceAt b p)
        f (m:ms) = case pieceAt b m of
          Nothing -> m:f ms
          Just (Piece pl _) -> if play == pl then [] else [m]
        f [] = []

-- checks if a player is allowed to castle to a certain side
castlingAllowed :: State -> BoardSide -> Bool
castlingAllowed state bs =
  kinfoMayCastle bs (stateKingInfo player state) &&
  clearPath (board state) (px1, y) (px2, y) &&
  not (any (isThreatened (board state) player) (between (bx1, y) (bx2, y)))
  where player = turn state
        y = if player == White then 0 else 7
        (px1, px2, bx1, bx2) = if bs == QueenSide then (0, 4, 1, 5) else (4, 7, 3, 7)

filterLegalMoves s = filter (not . leavesInCheck s)

-- TODO en passant, castling
legalMovesForPiece :: State -> (Position, Piece) -> [Move]
legalMovesForPiece s (pos, piece@(Piece p Pawn)) =
  filterLegalMoves s $ map (Move pos) $ concatMap (takeWhile legalPawnMove) (validMoves piece pos)
  where legalPawnMove to =
          if sameCol pos to then
          isNothing (pieceAt (board s) to)
          else
          pieceAtOwnedBy (board s) (nextPlayer p) to
legalMovesForPiece s (pos, piece@(Piece p k)) = filterLegalMoves s moves
  where moves = map (Move pos) $ concat $ filterMoves (board s) pos $ validMoves piece pos

leavesInCheck :: State -> Move -> Bool
leavesInCheck s m@(Move from to) =
  isThreatened (applyMove (board s) m) (turn s)
  (if kind (justPieceAt (board s) from) == King then to else stateKingPos (turn s) s)

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

isChecked :: State -> Player -> Bool
isChecked s p = isThreatened (board s) p (stateKingPos p s)

scoreForType :: PieceType -> Int
scoreForType Pawn = 1
scoreForType Knight = 3
scoreForType Bishop = 3
scoreForType Rook = 5
scoreForType Queen = 9
scoreForType King = 0

stateDoMove :: State -> Move -> State
stateDoMove s@(State b t w bl h) m@(Move from to) =
  State (applyMove b m) (nextPlayer t) wKing bKing
  (if pawnOpener m && kind (justPieceAt b from) == Pawn then Just (fst from) else Nothing)
  where (wKing, bKing) = updateKingInfo s m

scoreForPiece :: Piece -> Int
scoreForPiece (Piece White k) = scoreForType k
scoreForPiece (Piece Black k) = negate $ scoreForType k

scoreBoard :: Board -> Int
scoreBoard = sum . map (scoreForPiece . snd) . allPieces

legalMoves :: State -> [Move]
legalMoves s = castling ++ passants ++ concatMap (legalMovesForPiece s) (filter (\(pos, Piece p k) -> p == turn s) (allPieces (board s)))
  where castling = [castleMove (turn s) side | side <- [QueenSide, KingSide], castlingAllowed s side]
        (fromy, toy) = if (turn s) == White then (4, 5) else (3, 2)
        passants = [Move from to |
                    epx <- maybeToList (enPassant s),
                    x <- [epx-1, epx+1],
                    x <= 7 && x >= 0,
                    let (from, to) = ((x, fromy), (epx, toy)),
                    case pieceAt (board s) from of Nothing -> False
                                                   Just (Piece p k) -> k == Pawn && p == (turn s),
                    not (leavesInCheck s (Move from to))]


doAI :: State -> [AITree]
doAI state = map (\m -> let ns = stateDoMove state m in
                     AINode ns m (doAI ns))
             (legalMoves state)

findByMove :: [AITree] -> Move -> Maybe AITree
findByMove tree move = find (\node -> aiMove node == move) tree

scoreNode :: Int -> Int -> AITree -> Int
scoreNode maxDepth depth (AINode state move succ)
  | null succ =
    if isChecked state (turn state) then
      infinity depth
    else 0
  | depth >= maxDepth =
    scoreBoard (board state)
  | otherwise =
    best (pmap (scoreNode maxDepth (depth + 1)) succ)
    where (best, infinity) =
            if turn state == Black then (minimum, (maxBound -)) else (maximum, (minBound +))

scoreTree :: [AITree] -> Int -> [(AITree, Int)]
scoreTree tree maxDepth =
  pmap (\n -> (n, scoreNode maxDepth 0 n)) tree
  where nodeScore node = scoreNode maxDepth 0 node

bestForPlayer :: Player -> [(AITree, Int)] -> AITree
bestForPlayer p = fst . foldr1 (\a@(at, as) b@(bt, bs) -> if as `cmp` bs then a else b)
  where cmp = if p == White then (>) else (<)

pmap :: (a -> b) -> [a] -> [b]
pmap f [] = []
pmap f (x:xs) =
  let hd = f x
      rest = pmap f xs
  in rest `par` (hd `pseq` hd:rest)

-- Persistence

disambiguate :: Board -> Piece -> Move -> [Move] -> String
disambiguate board pc move@(Move from to) moves
  | null noDis = ""
  | null disByFile = file from
  | null disByRank = rank from
  | otherwise = square from
  where noDis = filterPieceTo board pc to moves
        disByFile = filter (\m@(Move f t) -> fst f == fst from) noDis
        disByRank = filter (\m@(Move f t) -> snd f == snd from) noDis

filterPieceTo :: Board -> Piece -> Position -> [Move] -> [Move]
filterPieceTo board pc dest =
  filter (\(Move from to) -> to == dest &&
           case pieceAt board to of Just otherpc -> pc == otherpc
                                    _ -> False)

annotateMove :: Board -> Move -> [Move] -> String
annotateMove board move@(Move from to) alts
  | kind fPiece == King && isJust castling =
    case castling of Just QueenSide -> "0-0-0"
                     Just KingSide -> "0-0"
  | otherwise =
    disamb ++ show (kind fPiece) ++ captureX ++ square to ++ pawn
   where fPiece = justPieceAt board from
         isCapture = isJust (pieceAt board to)
         castling = isCastling move
         disamb = disambiguate board fPiece move alts
         captureX = if isCapture then "x" else ""
         pawnEP = if not (sameCol from to) && not isCapture then "e.p." else ""
         pawnProm = if shouldPromote to then "=Q" else ""
         pawn = if kind fPiece == Pawn then pawnEP ++ pawnProm else ""

maxDepth :: Int
maxDepth = 2

checkIndicator :: Bool -> Bool -> String
checkIndicator checked hasMoves
  | checked && hasMoves = "+"
  | checked = "#"
  | otherwise = ""

winIndicator :: Player -> String
winIndicator White = "1-0"
winIndicator Black = "0-1"

treeToString :: State -> [AITree] -> String
treeToString = treeToStringRec 0

treeToStringRec :: Int -> State -> [AITree] -> String
treeToStringRec depth s t
  | null t =
    if checked then winIndicator (turn s) else "½-½"
  | depth <= maxDepth =
    intercalate ", " $ map (annotator depth s (map aiMove t)) t
  | otherwise = ""
  where checked = isChecked s (turn s)

annotateState :: State -> [Move] -> AITree -> String
annotateState prevState allMoves node =
  annotateMove (board prevState) move (delete move allMoves) ++
  checkIndicator (isChecked state (turn state)) (not (null succs))
  where move = aiMove node
        state = aiState node
        succs = aiSucc node

force :: [a] -> [a]
force (x:xs) = x `seq` (xs `seq` (x:xs))
force [] = []

appendHistory :: String -> State -> [AITree] -> AITree -> String
appendHistory str state alts chosen = force (str ++ sep ++ annotation)
  where sep = if null str then "" else ", "
        annotation = annotateState state (map aiMove alts) chosen

annotator :: Int -> State -> [Move] -> AITree -> String
annotator depth prevState allMoves node =
  annotateMove (board prevState) move (delete move allMoves) ++
    checkIndicator (isChecked state (turn state)) (not (null succs)) ++
    "(" ++ treeToStringRec (depth + 1) state succs ++ ")"
    where move = aiMove node
          state = aiState node
          succs = aiSucc node

file :: Position -> String
file (x,y) = [toEnum (fromEnum 'a' + x)]

rank :: Position -> String
rank (x,y) = [toEnum (fromEnum '1' + y)]

square :: Position -> String
square pos = file pos ++ rank pos
