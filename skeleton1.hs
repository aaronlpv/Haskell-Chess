import Data.Array
import Data.Char (isDigit, toLower, toUpper)
import Data.List (unfoldr)
import Data.String (unwords)

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

board0 :: Board
board0 =
  listArray
    (0, 63)
    (map (Just . Piece White) order ++
     1 `rowsOf` Just (Piece White Pawn) ++
     4 `rowsOf` Nothing ++
     1 `rowsOf` Just (Piece Black Pawn) ++
     map (Just . Piece Black) order)

posToIdx :: Position -> Int
posToIdx (x, y) = y * 8 + x

pieceAt :: Board -> Position -> Maybe Piece
pieceAt b p = b ! posToIdx p

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

printState :: State -> IO ()
printState (State b t _ _ _) = do
  let (bot, xs, ys) =
        if t == White
          then (['A'..'H'], [0..7], [7,6..0])
          else (['H','G'..'A'], [7,6..0], [0..7])
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

classicCompare :: Int -> Int -> Int
classicCompare x y = fromEnum (compare y x) - 1

between :: Position -> Position -> [Position]
between (x1, y1) to@(x2, y2) =
  takeWhile (\p -> p /= to) [(x1 + a * dx, y1 + a * dy) | a <- [1 ..]]
  where
    dx = (classicCompare x1 x2)
    dy = (classicCompare y1 y2)

clearPath :: Board -> Move -> Bool
clearPath b (Move from to) =
  all (\p -> pieceAt b p == Nothing) (between from to)

doMove :: Board -> Move -> Board
doMove b m@(Move from to) =
  let p = pieceAt b from in
    b // map (\(pos, piece) -> (posToIdx pos, piece)) [(from, Nothing), (to, p)]

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
            then if (clearPath b m || kind piece == Knight)
                   then ("Looks good", Just (State (doMove b m) (nextPlayer t) wc bc p))
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
