import Data.Array
import Data.Char (isDigit, toLower, toUpper)
import Data.List (unfoldr)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String (unwords)

type Command = String

type Message = String

type Position = (Int, Int)

data Move =
  Move Position Position
  deriving (Eq, Show)

data BoardSide
  = QueenSide
  | KingSide
  deriving (Enum, Eq, Ord, Show)

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Read, Enum, Eq, Ord, Show)

kindShow :: PieceType -> String
kindShow Pawn = "p"
kindShow Rook = "r"
kindShow Knight = "n"
kindShow Bishop = "b"
kindShow Queen = "q"
kindShow King = "k"

data Player
  = Black
  | White
  deriving (Eq, Show)

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black

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
showTile (Just p) = (show p)

type Board = Array Int (Maybe Piece)

data State =
  State
    { board :: Board
    , turn :: Player
    , wCanCastle :: (Bool, Bool)
    , bCanCastle :: (Bool, Bool)
    , passant :: Maybe Position
    }

readMove :: String -> Maybe Move
readMove [c1, c2, c3, c4]
  | all validChar [c1, c3] && all validDigit [c2, c4] =
    Just (Move (charToInt c1, digitToInt c2) (charToInt c3, digitToInt c4))
  | otherwise = Nothing
  where
    validChar c = c >= 'a' && c <= 'h'
    validDigit c = c >= '1' && c <= '8'
    charToInt c = fromEnum c - fromEnum 'a'
    digitToInt c = fromEnum c - fromEnum '1'
readMove _ = Nothing

order :: [PieceType]
order = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

rowsOf :: Int -> Maybe Piece -> [Maybe Piece]
rowsOf n = replicate (n * 8)

-- TODO: find out if positions can be used as indices directly
allPieces :: Board -> [(Position, Piece)]
allPieces b =
  map (\(idx, pc) -> ((idx `mod` 8, idx `div` 8), fromJust pc)) $
  filter (\(idx, pc) -> isJust pc) (assocs b)

board0 :: Board
board0 =
  listArray
    (0, 63)
    (map (Just . Piece White) order ++
     1 `rowsOf` Just (Piece White Pawn) ++
     4 `rowsOf` Nothing ++
     1 `rowsOf` Just (Piece Black Pawn) ++ map (Just . Piece Black) order)

posToIdx :: Position -> Int
posToIdx (x, y) = y * 8 + x

pieceAt :: Board -> Position -> Maybe Piece
pieceAt b p = b ! posToIdx p

squareColor :: Position -> Player
squareColor (x, y) =
  if even x == even y
    then Black
    else White

changeBoard :: Board -> [(Position, Maybe Piece)] -> Board
changeBoard b = (b //) . map (\(pos, piece) -> (posToIdx pos, piece))

moveToChange :: Board -> Move -> [(Position, Maybe Piece)]
moveToChange b (Move from to) = [(from, Nothing), (to, pieceAt b from)]

applyMove :: Board -> Move -> Board
applyMove b m = changeBoard b (moveToChange b m)

applyMoves :: Board -> [Move] -> Board
applyMoves b ms = changeBoard b (concatMap (moveToChange b) ms)

applyMovesAndChanges :: Board -> [Move] -> [(Position, Maybe Piece)] -> Board
applyMovesAndChanges b ms cs =
  changeBoard b (cs ++ concatMap (moveToChange b) ms)

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

printState :: State -> IO ()
printState (State b t _ _ _) = do
  let (bot, xs, ys) =
        if t == White
          then (['A' .. 'H'], [0 .. 7], [7,6 .. 0])
          else (['H','G' .. 'A'], [7,6 .. 0], [0 .. 7])
  mapM_
    (putStrLn . (\(i, s) -> (show $ i + 1) ++ " | " ++ s))
    (zip
       ys
       (map
          (unwords . (map (showTile . pieceAt b)))
          [[(x, y) | x <- xs] | y <- ys]))
  putStrLn "--+----------------"
  putStrLn $ "  | " ++ interleave bot (repeat ' ')

state0 :: State
state0 = State board0 White (True, True) (True, True) Nothing

sameRow :: Position -> Position -> Bool
sameRow (x1, y1) (x2, y2) = y1 == y2

sameCol :: Position -> Position -> Bool
sameCol (x1, y1) (x2, y2) = x1 == x2

diff :: Int -> Int -> Int
diff x y = abs (x - y)

sameDiag :: Position -> Position -> Bool
sameDiag (x1, y1) (x2, y2) = diff x1 x2 == diff y1 y2

oneStep :: Move -> Bool
oneStep (Move (x1, y1) (x2, y2)) = diff x1 x2 <= 1 && diff y1 y2 <= 1

pawnOpener :: Move -> Bool
pawnOpener (Move from@(x1, y1) to@(x2, y2)) =
  sameCol from to && (y1, y2) `elem` [(1, 3), (6, 4)]

validPawnMove :: Move -> Bool
validPawnMove m = oneStep m || pawnOpener m

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

validMoves :: PieceType -> Position -> [Position]
validMoves Knight (x, y) =
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
validMoves _ _ = []

validCapture :: Piece -> Move -> Bool
validCapture p@(Piece _ Pawn) m@(Move (x1, y1) (x2, y2)) =
  validMove p m && x1 /= x2
validCapture p m = validMove p m

classicCompare :: Int -> Int -> Int
classicCompare y x = fromEnum (compare y x) - 1

ray :: Position -> (Int, Int) -> [Position]
ray (x, y) (dx, dy) =
  takeWhile validPos [(x + a * dx, y + a * dy) | a <- [1 ..]]

straightRays :: Position -> [[Position]]
straightRays pos = map (ray pos) [(0, 1), (1, 0), (0, -1), (-1, 0)]

diagRays :: Position -> [[Position]]
diagRays pos = map (ray pos) [(1, 1), (-1, -1), (1, -1), (-1, 1)]

raysFrom :: Position -> [[Position]]
raysFrom pos = straightRays pos ++ diagRays pos

validPos :: Position -> Bool
validPos (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

isBehind :: Position -> Position -> Bool
isBehind p1@(x1, y1) p2@(x2, y2)
  | not (sameCol p1 p2) = False
  | y2 <= 3 = y1 == y2 - 1
  | otherwise = y1 == y2 + 1

between :: Position -> Position -> [Position]
between from@(x1, y1) to@(x2, y2) = take num (ray from (dx, dy))
  where
    num = max (diff x1 x2) (diff y1 y2) - 1
    dx = (classicCompare x2 x1)
    dy = (classicCompare y2 y1)

captureRays :: Position -> [[Position]]
captureRays from = raysFrom from ++ (map (: []) $ validMoves Knight from)

underAttack :: Board -> Player -> Position -> Bool
underAttack b pl pos =
  any dangerous $
  map (map (\apos -> (apos, b `pieceAt` apos))) (captureRays pos)
  where
    dangerous [] = False
    dangerous ((_, Nothing):xs) = dangerous xs
    dangerous (((ppos, Just p@(Piece pl2 knd))):xs) =
      pl /= pl2 && validCapture p (Move ppos pos)

clearPath :: Board -> Position -> Position -> Bool
clearPath b from to = all (\p -> pieceAt b p == Nothing) (between from to)

isPromotion :: Piece -> Move -> Bool
isPromotion (Piece p Pawn) (Move from (x2, y2)) = y2 `elem` [0, 7]
isPromotion _ _ = False

isCastling :: Move -> Maybe BoardSide
isCastling (Move (4, 7) (2, 7)) = Just QueenSide
isCastling (Move (4, 7) (6, 7)) = Just KingSide
isCastling (Move (4, 0) (2, 0)) = Just QueenSide
isCastling (Move (4, 0) (6, 0)) = Just KingSide
isCastling _ = Nothing

castleRookMove :: Player -> BoardSide -> Move
castleRookMove Black QueenSide = Move (0, 7) (3, 7)
castleRookMove Black KingSide = Move (7, 7) (5, 7)
castleRookMove White QueenSide = Move (0, 0) (3, 0)
castleRookMove White KingSide = Move (7, 0) (5, 0)

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

doCastling :: State -> Move -> (Message, Maybe State)
doCastling s@(State b t wc bc p) m@(Move from to) =
  case isCastling m of
    Nothing -> (error "Invalid castling move in doCastling")
    Just side ->
      if castlingAllowed s t side
        then ( "Castled!"
             , Just
                 (State
                    (applyMoves b [m, castleRookMove t side])
                    (nextPlayer t)
                    wc
                    bc
                    Nothing))
        else ("You may not castle at this time", Just s)

doPawnMove :: State -> Move -> Piece -> Maybe Piece -> (Message, Maybe State)
doPawnMove s@(State b t wc bc p) m@(Move from to) frompiece topiece
  | pawnOpener m && isNothing topiece && clearPath b from to =
    ("", Just (State (applyMove b m) (nextPlayer t) wc bc (Just to)))
  | validCapture frompiece m &&
      case p of
        Nothing -> False
        Just passpos -> to `isBehind` passpos =
    ( "Just passing by"
    , Just
        (State
           (applyMovesAndChanges b [m] [(fromJust p, Nothing)])
           (nextPlayer t)
           wc
           bc
           Nothing))
  | otherwise = doMove s m frompiece topiece

doMove :: State -> Move -> Piece -> Maybe Piece -> (Message, Maybe State)
doMove s@(State b t wc bc p) m@(Move from to) fromp@(Piece frp frk) topiece =
  if (frk == Knight || clearPath b from to) &&
     case topiece of
       Nothing -> True
       Just (Piece top _) -> top /= t
    then ("", Just (State (applyMove b m) (nextPlayer t) wc bc Nothing))
    else ("Path blocked", Just s)

step :: State -> Command -> (Message, Maybe State)
step s c =
  case readMove c of
    Just m@(Move from to) ->
      update s m (pieceAt (board s) from) (pieceAt (board s) to)
    _ -> ("Invalid command", Just s)

update :: State -> Move -> Maybe Piece -> Maybe Piece -> (Message, Maybe State)
update s m Nothing topiece = ("No piece there", Just s)
update s@(State b t wc bc p) m@(Move from to) (Just fromp@(Piece frp frk)) topiece
  | frp /= t = ("Not your piece", Just s)
  | frk == King && isJust (isCastling m) = doCastling s m
  | not (validMove fromp m) = ("Invalid " ++ show frk ++ " move", Just s)
  | frk == Pawn = doPawnMove s m fromp topiece
  | otherwise = doMove s m fromp topiece

insufMaterial :: Board -> Bool
insufMaterial b = loop (allPieces b) False False False False 0 0
  where
    loop ((_, (Piece Black Knight)):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      loop pcs wbBshp wwBshp bbBshp bwBshp (bKnghts + 1) wKnghts
    loop ((_, (Piece White Knight)):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      loop pcs wbBshp wwBshp bbBshp bwBshp bKnghts (wKnghts + 1)
    loop ((pos, (Piece White Bishop)):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      if squareColor pos == White
        then loop pcs wbBshp True bbBshp bwBshp bKnghts wKnghts
        else loop pcs True wwBshp bbBshp bwBshp bKnghts wKnghts
    loop ((pos, (Piece Black Bishop)):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
      if squareColor pos == White
        then loop pcs wbBshp wwBshp bbBshp True bKnghts wKnghts
        else loop pcs wbBshp wwBshp True bwBshp bKnghts wKnghts
    loop ((_, (Piece _ King)):pcs) wbBshp wwBshp bbBshp bwBshp bKnghts wKnghts =
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
      (sum $ map fromEnum [wbBshp, wwBshp, bbBshp, bwBshp]) == 1 -- King vs King and bishop

endgameCheck :: State -> State -> (Message, Maybe State)
endgameCheck old new = ("OK", Just new)

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
