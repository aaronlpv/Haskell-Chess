import Types
import Position hiding (between)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char


data ParsedState = ParsedState [ParsedMove] [ParsedNode]
  deriving (Show)

data ParsedNode = ParsedNode ParsedMove ParsedNext
  deriving (Show)

data ParsedNext = MoreNodes [ParsedNode] | EndOfExploration | GameEnd
  deriving (Show)

data Disamb = Disamb (Maybe Int) (Maybe Int)

data PawnExtra = PawnEnPassant | PawnPromotion
  deriving (Show)

data ParsedMove = ParsedMove Disamb PieceType Bool Position (Maybe PawnExtra) Bool | ParsedCastling BoardSide
  deriving (Show)

instance Show Disamb where
  show (Disamb x y) = showMaybe x ++ showMaybe y
    where showMaybe (Just a) = show a
          showMaybe Nothing = ""

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

parsePiece :: Char -> PieceType
parsePiece 'K' = King
parsePiece 'Q' = Queen
parsePiece 'R' = Rook
parsePiece 'B' = Bishop
parsePiece 'N' = Knight

pNotPawn :: Parser PieceType
pNotPawn = do c <- oneOf "KQRBN"
              return (parsePiece c)

pPiece :: Parser PieceType
pPiece = spaces >> option Pawn pNotPawn

pPawnExtra :: Parser (Maybe PawnExtra)
pPawnExtra = spaces >>
             (option Nothing $ choice [string "=Q" >> return (Just PawnPromotion),
                                       string "e.p." >> return (Just PawnEnPassant)])

pCheck :: Parser Bool
pCheck = spaces >> (option False $ (oneOf "+#" >> return True))

pCapture :: Parser Bool
pCapture = spaces >> (option False (char 'x' >> return True))

pEnd :: Parser ParsedNext
pEnd = do spaces
          choice [string "1-0", string "0-1", string "½-½"]
          return GameEnd

pDisamb :: Parser Disamb
pDisamb = spaces >> choice [pSquare >>= (\(f, r) -> return (Disamb (Just f) (Just r))),
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
                   chk <- pCheck
                   return $ ParsedMove dis pc cap sq pe chk

pNodes :: Parser [ParsedNode]
pNodes = sepBy1 pNode (spaces >> char ',' >> spaces)

pNode :: Parser ParsedNode
pNode = do step <- pStep
           next <- parens $ option EndOfExploration (choice [pEnd, (pNodes >>= return . MoreNodes)])
           return $ ParsedNode step next

pState :: Parser ParsedState
pState = do spaces
            steps <- sepBy1 pStep (spaces >> char ',')
            nodes <- parens pNodes
            return $ ParsedState steps nodes
