import Data.Maybe (fromJust)
import Codec.BMP
import Control.Monad
import Data.Either (fromRight)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Board
import Position
import Types
import Move
import AI

windowSize :: Int
windowSize = 856

fwindowSize :: Float
fwindowSize = fromIntegral windowSize

unit :: Int
unit = 213

funit :: Float
funit = fromIntegral unit

data GState = PickPlayer | Game State (Maybe Position) | GameOver State

gstate0 :: GState
gstate0 = Game state0 Nothing

loadSprites :: BitmapData -> [[Picture]]
loadSprites bmp = [loadSp 0, loadSp unit]
  where
    loadSp y =
      [ Scale (1 / funit) (1 / funit) $
      bitmapSection (Rectangle (x * unit, y) (unit, unit)) bmp
      | x <- [0,1 .. 7]
      ]

spriteForPiece :: [[Picture]] -> Piece -> Picture
spriteForPiece sprites (Piece p k) = (sprites !! fromEnum p) !! fromEnum k

convertPosition :: Player -> Position -> Position
convertPosition White p = p
convertPosition Black (x,y) = (7-x, 7-y)

displayState :: [[Picture]] -> GState -> Picture
displayState sprites (Game s _) =
  Color black $
  Scale (funit / 2) (funit / 2) $
  Translate (-3.5) (-3.5) (pictures $ grid ++ pieces)
  where
    pieces =
      map
        (\(pos, piece) ->
            case convertPosition (turn s) pos of (x,y) -> Translate (fromIntegral x) (fromIntegral y) (spriteForPiece sprites piece))
        (allPieces (board s))
    grid =
      [ Translate (fromIntegral x) (fromIntegral y) (rectangleSolid 1 1)
      | x <- [0,1 .. 7]
      , y <- [0,1 .. 7]
      , (odd x == odd y) == (turn s == White)
      ]
displayState sprites PickPlayer =
  Pictures
  [Translate (-fwindowSize/4.5) (fwindowSize/5) $ Text "Chess",
   Scale 0.5 0.5 $ Translate (-fwindowSize/2.15) (-fwindowSize/6) $ Text "Pick a side",
   Translate (-fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ spriteForPiece sprites (Piece White King),
   Translate (fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ spriteForPiece sprites (Piece Black King)]
displayState _ _ = error "not implemented" -- FIXME

handleEvent :: Event -> GState -> GState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) state@(Game s from)
  = case from of Nothing -> if pieceAtOwnedBy (board s) (turn s) cpos then Game s (Just cpos) else state
                 Just fromPos ->
                   if (Move fromPos cpos) `elem` (legalMoves s)
                   then let ns = (stateDoMove s (Move fromPos cpos)) in
                     Game (stateDoMove ns (aiMove (bestForPlayer (turn ns) (scoreTree (doAI ns) 2)))) Nothing
                   else Game s Nothing
  where cpos = convertPosition (turn s) (floor ((x + fwindowSize / 2) / funit * 2),
                                  floor ((y + fwindowSize / 2) / funit * 2))

handleEvent (EventKey (MouseButton LeftButton) Up _ _) PickPlayer = gstate0
handleEvent _ s = s

main = do
  bmp <-
    readBMP
      "/home/aaron/Documents/univ/1MA/Func/assignments/chess/code/sprites.bmp"
  let sprites =
        loadSprites $
        bitmapDataOfBMP (fromRight (error "Sprites not found!") bmp)
  interactIO
    (InWindow "Chess" (windowSize, windowSize) (10, 10))
    white
    PickPlayer
    (return . displayState sprites)
    (\e s -> return $ handleEvent e s)
    (\_ -> return ())
