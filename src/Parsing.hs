module Parsing
  ( parseDotChess
  , Disamb(..)
  , PawnExtra(..)
  , ParsedMove(..)
  , CheckStatus(..)
  , ParsedNode(..)
  , ParsedNext(..)
  , ParsedState(..)
  ) where

import Control.DeepSeq
import Position hiding (between)
import Types

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

import Data.Maybe (maybe)

data ParsedState =
  ParsedState [ParsedMove] [ParsedNode]
  deriving (Show)

data ParsedNode =
  ParsedNode ParsedMove ParsedNext
  deriving (Show)

data ParsedNext
  = MoreNodes [ParsedNode]
  | EndOfExploration
  | GameEnd
  deriving (Show)

data Disamb =
  Disamb (Maybe Int) (Maybe Int)

data CheckStatus
  = NoCheck
  | Check
  | CheckMate

data PawnExtra
  = PawnEnPassant
  | PawnPromotion

data ParsedMove
  = ParsedMove Disamb PieceType Bool Position (Maybe PawnExtra) CheckStatus
  | ParsedCastling BoardSide

-- added in an attempt to solve the move history space leak
-- unfortunately did not help
instance NFData ParsedMove where
  rnf (ParsedCastling side) = side `seq` ()
  rnf (ParsedMove dis pc cap sq paw ch) =
    rnf dis `seq` pc `seq` cap `seq` rnf sq `seq` rnf paw `seq` ch `seq` ()

instance NFData Disamb where
  rnf (Disamb x y) = rnf x `seq` rnf y `seq` ()

instance NFData PawnExtra where
  rnf = rwhnf

file :: Int -> String
file x = [toEnum (fromEnum 'a' + x)]

rank :: Int -> String
rank y = [toEnum (fromEnum '1' + y)]

square (x, y) = file x ++ rank y

instance Show Disamb where
  show (Disamb x y) = maybe "" file x ++ maybe "" rank y

instance Show PawnExtra where
  show PawnEnPassant = "e.p."
  show PawnPromotion = "=Q"

instance Show CheckStatus where
  show NoCheck = ""
  show Check = "+"
  show CheckMate = "#"

instance Show ParsedMove where
  show (ParsedMove dis pc cap sq pe ch) =
    show dis ++
    show pc ++
    (if cap
       then "x"
       else "") ++
    square sq ++ maybe "" show pe ++ show ch
  show (ParsedCastling QueenSide) = "0-0-0"
  show (ParsedCastling KingSide) = "0-0"

-- something between parentheses and spaces
parens :: Parser a -> Parser a
parens = between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces)

pFile :: Parser Int
pFile = do
  spaces
  c <- oneOf ['a' .. 'h']
  return (fromEnum c - fromEnum 'a')

pRank :: Parser Int
pRank = do
  spaces
  c <- oneOf ['1' .. '8']
  return (fromEnum c - fromEnum '1')

pSquare :: Parser Position
pSquare = do
  f <- pFile
  r <- pRank
  return (f, r)

pPiece :: Parser PieceType
pPiece =
  choice
    [ char 'K' >> return King
    , char 'Q' >> return Queen
    , char 'R' >> return Rook
    , char 'B' >> return Bishop
    , char 'N' >> return Knight
    , return Pawn
    ]

-- parses one of the special pawn indicators
pPawnExtra :: Parser (Maybe PawnExtra)
pPawnExtra =
  spaces >>
  choice
    [ string "=Q" >> return (Just PawnPromotion)
    , string "e.p." >> return (Just PawnEnPassant)
    , return Nothing
    ]

-- parses a check indicator
pCheck :: Parser CheckStatus
pCheck =
  spaces >>
  choice
    [char '+' >> return Check, char '#' >> return CheckMate, return NoCheck]

-- parses the "x" and returns true if found
pCapture :: Parser Bool
pCapture = spaces >> option False (char 'x' >> return True)

-- parses a game end, we do not actually care which one
-- we do not store this info
pEnd :: Parser ParsedNext
pEnd = do
  spaces
  choice [string "1-0", string "0-1", string "½-½"]
  return GameEnd

-- parses a disambiguation
pDisamb :: Parser Disamb
pDisamb =
  spaces >>
  choice
    [ try pSquare >>= (\(f, r) -> return (Disamb (Just f) (Just r)))
    , pFile >>= (\s -> return (Disamb (Just s) Nothing))
    , pRank >>= (\r -> return (Disamb Nothing (Just r)))
    ]

-- parses a castling move
pCastling :: Parser ParsedMove
pCastling = do
  spaces
  string "0-0"
  option
    (ParsedCastling KingSide)
    (string "-0" >> return (ParsedCastling QueenSide))

-- parses a step
-- just a square is a special case because we initially parse it as a disambiguation
-- which is why we need the try
pStep :: Parser ParsedMove
pStep =
  pCastling <|> try (pDisamb >>= pStepRest) <|>
  pStepRest (Disamb Nothing Nothing)

-- parses the rest of the step
pStepRest :: Disamb -> Parser ParsedMove
pStepRest dis = do
  pc <- pPiece
  cap <- pCapture
  sq <- pSquare
  pe <- pPawnExtra
  ch <- pCheck
  return $ ParsedMove dis pc cap sq pe ch

pNodes :: Parser [ParsedNode]
pNodes = sepBy1 pNode (spaces >> char ',' >> spaces)

pNode :: Parser ParsedNode
pNode = do
  step <- pStep
  next <- parens $ option EndOfExploration (choice [pEnd, MoreNodes <$> pNodes])
  return $ ParsedNode step next

pState :: Parser ParsedState
pState = do
  spaces
  steps <- sepBy1 pStep (try (spaces >> char ',' >> spaces))
  spaces
  nodes <- parens pNodes
  return $ ParsedState steps nodes

-- parses the contents of a .chess file
parseDotChess :: String -> String -> Either ParseError ParsedState
parseDotChess = parse pState
