import Data.Char (isDigit, toLower, toUpper)
import Data.String (unwords)
import Data.List (unfoldr)
import Data.Array.IArray

type Command = String

type Message = String

type Position = (Int, Int)

data Move =
  Move Position Position
  deriving (Eq, Show)

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Read, Enum, Eq, Ord, Show)

pieceShow :: PieceType -> String
pieceShow Pawn = "p"
pieceShow Rook = "r"
pieceShow Knight = "n"
pieceShow Bishop = "b"
pieceShow Queen = "q"
pieceShow King = "k"

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
  show (Piece Black r) = pieceShow r
  show (Piece White r) = map toUpper $ pieceShow r

type Board = [[Maybe Piece]]

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

rowOf :: Maybe Piece -> [Maybe Piece]
rowOf = replicate 8

order :: [PieceType]
order = [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]

board0 :: Board
board0 =
  [map (Just . Piece White) order, rowOf $ Just (Piece White Pawn)] ++
  replicate 4 (rowOf Nothing) ++
  [rowOf $ Just (Piece Black Pawn), map (Just . Piece Black) order]

rotateBoard :: Board -> Board
rotateBoard = reverse . map reverse

pieceAt :: Board -> Position -> Maybe Piece
pieceAt b (x, y) = (b !! y) !! x

colChars :: Player -> String
colChars White = "  | A B C D E F G H"
colChars Black = "  | H G F E D C B A"

printState :: State -> IO ()
printState (State b White _ _ _) =
  printBoard (rotateBoard b) [8,7 .. 1] (colChars White)
printState (State b Black _ _ _) = printBoard b [1 .. 8] (colChars Black)

showRow :: (Int, [Maybe Piece]) -> String
showRow (x, ps) =
  show x ++
  " | " ++
  unwords
    (map
       (\p ->
          case p of
            Just p -> show p
            Nothing -> ".")
       ps)

printBoard :: Board -> [Int] -> String -> IO ()
printBoard b rownums bottom = do
  mapM_ (putStrLn . showRow) (zip rownums b)
  putStrLn "--+----------------"
  putStrLn bottom

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

classicCompare :: Int -> Int -> Int
classicCompare x y = fromEnum (compare y x) - 1

between :: Position -> Position -> [Position]
between (x1, y1) to@(x2, y2) =
  takeWhile (\p -> p /= to) [(x1 + a * dx, y1 + a * dy) | a <- [1..]]
  where dx = (classicCompare x1 x2)
        dy = (classicCompare y1 y2)

clearPath :: Board -> Move -> Bool
clearPath b (Move from to) = all (\p -> pieceAt b p == Nothing) (between from to)

step :: State -> Command -> (Message, Maybe State)
step s c =
  case readMove c of
    Just m -> update s m
    _ -> ("Invalid command", Just s)

update :: State -> Move -> (Message, Maybe State)
update s@(State b t wc bc p) m@(Move from to) =
  let mpiece = pieceAt b from
   in case mpiece of
        Just piece ->
          if validMove piece m
            then if (clearPath b m ||kind piece == Knight) then
                   ("Looks good", Just (State b (nextPlayer t) wc bc p))
                 else ("Path blocked", Just s)
            else ("Invalid " ++ show (kind piece) ++ " move", Just s)
        Nothing -> ("No piece there", Just s)

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
