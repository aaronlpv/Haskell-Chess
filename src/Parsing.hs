import Types
import Position hiding (between)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import Data.Maybe (maybe)


data ParsedState = ParsedState [ParsedMove] [ParsedNode]
  deriving (Show)

data ParsedNode = ParsedNode ParsedMove ParsedNext
  deriving (Show)

data ParsedNext = MoreNodes [ParsedNode] | EndOfExploration | GameEnd
  deriving (Show)

data Disamb = Disamb (Maybe Int) (Maybe Int)

data PawnExtra = PawnEnPassant | PawnPromotion

data ParsedMove = ParsedMove Disamb PieceType Bool Position (Maybe PawnExtra) Bool | ParsedCastling BoardSide

file :: Int -> String
file x = [toEnum (fromEnum 'a' + x)]

rank :: Int -> String
rank y = [toEnum (fromEnum '1' + y)]

square (x,y) = file x ++ rank y

instance Show Disamb where
  show (Disamb x y) = (maybe "" file x) ++ (maybe "" rank y)

instance Show PawnExtra where
  show PawnEnPassant = "e.p."
  show PawnPromotion = "=Q"

instance Show ParsedMove where
  show (ParsedMove dis pc cap sq pe ch) =
    show dis ++ show pc ++ (if cap then "x" else "") ++
    square sq ++ maybe "" show pe ++ (if ch then "+" else "")

parens :: Parser a -> Parser a
parens = between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces)

pFile :: Parser Int
pFile = do spaces
           c <- oneOf ['a'..'h']
           return (fromEnum c - fromEnum 'a')

pRank :: Parser Int
pRank = do spaces
           c <- oneOf ['1'..'8']
           return (fromEnum c - fromEnum '1')

pSquare :: Parser Position
pSquare = do f <- pFile
             r <- pRank
             return (f, r)

pNotPawn :: Parser PieceType
pNotPawn = choice [char 'K' >> return King,
                   char 'Q' >> return Queen,
                   char 'R' >> return Rook,
                   char 'B' >> return Bishop,
                   char 'N' >> return Knight,
                   return Pawn]

pPiece :: Parser PieceType
pPiece = spaces >> option Pawn pNotPawn

pPawnExtra :: Parser (Maybe PawnExtra)
pPawnExtra = spaces >>
             choice [string "=Q" >> return (Just PawnPromotion),
                     string "e.p." >> return (Just PawnEnPassant),
                     return Nothing]

pCheck :: Parser Bool
pCheck = spaces >> option False (oneOf "+#" >> return True)

pCapture :: Parser Bool
pCapture = spaces >> option False (char 'x' >> return True)

pEnd :: Parser ParsedNext
pEnd = do spaces
          choice [string "1-0", string "0-1", string "½-½"]
          return GameEnd

pDisamb :: Parser Disamb
pDisamb = spaces >> choice [try pSquare >>= (\(f, r) -> return (Disamb (Just f) (Just r))),
                            pFile >>= (\s -> return (Disamb (Just s) Nothing)),
                            pRank >>= (\r -> return (Disamb Nothing (Just r)))]

pCastling :: Parser ParsedMove
pCastling = do spaces
               string "0-0"
               option (ParsedCastling KingSide) (string "-0" >> return (ParsedCastling QueenSide))

pStep :: Parser ParsedMove
pStep = pCastling
        <|> try (pDisamb >>= pStepRest)
        <|> pStepRest (Disamb Nothing Nothing)

pStepRest dis = do pc <- pPiece
                   cap <- pCapture
                   sq <- pSquare
                   pe <- pPawnExtra
                   ch <- pCheck
                   return $ ParsedMove dis pc cap sq pe ch

pNodes :: Parser [ParsedNode]
pNodes = sepBy1 pNode (spaces >> char ',' >> spaces)

pNode :: Parser ParsedNode
pNode = do step <- pStep
           next <- parens $ option EndOfExploration (choice [pEnd, MoreNodes <$> pNodes])
           return $ ParsedNode step next

pState :: Parser ParsedState
pState = do spaces
            steps <- sepBy1 pStep (try (spaces >> char ',' >> spaces))
            spaces
            nodes <- parens pNodes
            return $ ParsedState steps nodes

parseDotChess :: String -> Either ParseError ParsedState
parseDotChess = parse pState "(unknown)"
