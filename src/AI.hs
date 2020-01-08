module AI(AITree(..), doAI, findByMove, scoreTree, bestForPlayer) where
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

scoreForType :: PieceType -> Int
scoreForType Pawn = 1
scoreForType Knight = 3
scoreForType Bishop = 3
scoreForType Rook = 5
scoreForType Queen = 9
scoreForType King = 0

scoreForPiece :: Piece -> Int
scoreForPiece (Piece White k) = scoreForType k
scoreForPiece (Piece Black k) = negate $ scoreForType k

scoreBoard :: Board -> Int
scoreBoard = sum . map (scoreForPiece . snd) . allPieces

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
