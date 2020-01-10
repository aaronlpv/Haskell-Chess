module AI(AITree(..), doAI, findByMove, scoreTree, bestForPlayer, annotateStep, treeToString, applyAnnotated) where
import Types
import Board
import Position
import Chess
import Move
import Parsing

import Data.Maybe (fromJust, isNothing, isJust, maybeToList)
import Data.List (find, intercalate, delete)
import Control.Parallel (par, pseq)

data AITree = AINode {
  aiState :: State,
  aiMove :: Move,
  aiSucc :: [AITree] }

-- max depth for exploration and saving
maxDepth :: Int
maxDepth = 3

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

pmap :: (a -> b) -> [a] -> [b]
pmap f [] = []
pmap f (x:xs) =
  let hd = f x
      rest = pmap f xs
  in rest `par` (hd `pseq` hd:rest)

scoreNode :: Int -> AITree -> Int
scoreNode depth (AINode state move succ)
  | null succ =
    if isChecked state then
      infinity depth
    else 0
  | depth >= maxDepth =
    scoreBoard (board state)
  | otherwise =
    best (pmap (scoreNode (depth + 1)) succ)
    where (best, infinity) =
            if turn state == Black then (minimum, (maxBound -)) else (maximum, (minBound +))

scoreTree :: [AITree] -> [(AITree, Int)]
scoreTree tree =
  pmap (\n -> (n, scoreNode 1 n)) tree

bestForPlayer :: Player -> [(AITree, Int)] -> AITree
bestForPlayer p = fst . foldr1 (\a@(at, as) b@(bt, bs) -> if as `cmp` bs then a else b)
  where cmp = if p == White then (>) else (<)

-- Persistence

filterPieceTo :: Board -> PieceType -> Position -> [Move] -> [Move]
filterPieceTo board pck dest =
  filter (\(Move from to) -> to == dest &&
           case pieceAt board from of Just (Piece _ k) -> pck == k
                                      _ -> False)

disambiguate :: Board -> Piece -> Move -> [Move] -> Disamb
disambiguate board pc move@(Move from@(fx,fy) to) moves
  | null noDis = (Disamb Nothing Nothing)
  | null disByFile = (Disamb (Just fx) Nothing)
  | null disByRank = (Disamb Nothing (Just fy))
  | otherwise = (Disamb (Just fx) (Just fy))
  where noDis = filterPieceTo board (kind pc) to moves
        disByFile = filter (\m@(Move f t) -> fst f == fst from) noDis
        disByRank = filter (\m@(Move f t) -> snd f == snd from) noDis

annotateMove :: Board -> Move -> [Move] -> CheckStatus -> ParsedMove
annotateMove board move@(Move from to) alts check
  | kind fPiece == King && isJust castling =
    case castling of Just side -> ParsedCastling side
  | otherwise =
    ParsedMove disamb (kind fPiece) isCapture to pawn check
   where fPiece = justPieceAt board from
         isCapture = isJust (pieceAt board to)
         castling = isCastling move
         disamb = disambiguate board fPiece move alts
         pawnEP = not (sameCol from to) && not isCapture
         pawnProm = shouldPromote to
         pawn = if kind fPiece == Pawn then pawnIndicator pawnEP pawnProm else Nothing

checkIndicator :: Bool -> Bool -> CheckStatus
checkIndicator checked hasMoves
  | checked && hasMoves = Check
  | checked = CheckMate
  | otherwise = NoCheck

pawnIndicator :: Bool -> Bool -> Maybe PawnExtra
pawnIndicator passant promotion
  | passant = Just PawnEnPassant
  | promotion = Just PawnPromotion
  | otherwise = Nothing

winIndicator :: Player -> String
winIndicator White = "1-0"
winIndicator Black = "0-1"

treeToString :: State -> [AITree] -> String
treeToString = treeToStringRec 1

treeToStringRec :: Int -> State -> [AITree] -> String
treeToStringRec depth state succs
  | null succs =
    if isChecked state then winIndicator (turn state) else "½-½"
  | depth < maxDepth =
    intercalate ", " $ map recurse succs
  | otherwise = ""
  where recurse node = show (annotateStep state node succs) ++
          "(" ++ treeToStringRec (depth + 1) (aiState node) (aiSucc node) ++ ")"

annotateStep :: State -> AITree -> [AITree] -> ParsedMove
annotateStep prevState node allNodes =
  annotateMove (board prevState) move (delete move (map aiMove allNodes)) (checkIndicator (isChecked state) (not (null succs)))
  where move = aiMove node
        state = aiState node
        succs = aiSucc node

applyAnnotated :: State -> [ParsedMove] -> State
applyAnnotated state ((ParsedCastling side):ms) =
  stateDoMove state (castleMove (turn state) side)
applyAnnotated state ((ParsedMove dis pc _ dest _ _):ms) =
  applyAnnotated (stateDoMove state (fromJust (find (matchDis dis) pcTo))) ms
  where legals = legalMoves state
        pcTo = filterPieceTo (board state) pc dest legals
        matchDis (Disamb Nothing Nothing) =
          \_ -> True
        matchDis (Disamb Nothing (Just y)) =
          \(Move (_,b) _) -> b == y
        matchDis (Disamb (Just x) Nothing) =
          \(Move (a,_) _) -> a == x
        matchDis (Disamb (Just x) (Just y)) =
          \(Move (a,b) _) -> a == x && b == y
applyAnnotated s [] = s
