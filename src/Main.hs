import Codec.BMP
import Control.Monad
import Data.Either (fromRight)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Board
import Position
import Types
import Move
import Chess
import AI
import Utils

-- changing this scales everything
windowSize :: Int
windowSize = 856

fwindowSize :: Float
fwindowSize = fromIntegral windowSize

unit :: Int
unit = 213

funit :: Float
funit = fromIntegral unit

pieceScale :: Float
pieceScale = fwindowSize / 8

scalePiece :: Picture -> Picture
scalePiece = Scale pieceScale pieceScale

scaleText :: Picture -> Picture
scaleText = Scale (fwindowSize / 1712) (fwindowSize / 1712)

scaleTitle :: Picture -> Picture
scaleTitle = Scale (fwindowSize / 856) (fwindowSize / 856)

relTranslate :: Float -> Float -> Picture -> Picture
relTranslate x y = Translate (fwindowSize / x) (fwindowSize / y)

data GState = PickPlayer | Game State [AITree] String (Maybe Position) | GameOver State String

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

displayState :: [[Picture]] -> State -> Picture
displayState sprites s =
  Color black $
  scalePiece $
  Translate (-3.5) (-3.5) (pictures $ grid ++ pieces)
  where
    grid =
      [ Translate (fromIntegral x) (fromIntegral y) (rectangleSolid 1 1)
      | x <- [0,1 .. 7]
      , y <- [0,1 .. 7]
      , (odd x == odd y) == (turn s == White)
      ]
    pieces =
      map
        (\(pos, piece) ->
           let (x, y) = convertPosition (turn s) pos in
            Translate (fromIntegral x) (fromIntegral y) (spriteForPiece sprites piece))
        (allPieces (board s))

displayGState :: [[Picture]] -> GState -> Picture
displayGState sprites (Game s _ _ _) = displayState sprites s
displayGState sprites PickPlayer =
  Pictures
  [relTranslate (-4.5) 5 $ scaleTitle (Text "Chess"),
   relTranslate (-4.5) (-20) $ scaleText (Text "Pick a side"),
   relTranslate (-5) (-4) $ scalePiece $ spriteForPiece sprites (Piece White King),
   relTranslate 5 (-4) $ scalePiece $ spriteForPiece sprites (Piece Black King)]
displayGState sprites (GameOver state msg) =
  Pictures
  [displayState sprites state,
   Color (greyN 0.3) (rectangleSolid fwindowSize (fwindowSize / 7)),
   relTranslate (-4.5) (-30) $ scaleText $ Color white $ Text msg]

execMove :: GState -> AITree -> GState
execMove (Game state ai hist _) tree =
  endGameCheck $ Game (aiState tree) (aiSucc tree) hist Nothing--(appendHistory hist state ai tree) Nothing
execMove _ _ = error "Can only move in game"

aiDoMove :: GState -> GState
aiDoMove game@(Game state ai hist _) =
  execMove game (bestForPlayer (turn state) (scoreTree ai))
aiDoMove s = s

endGameCheck :: GState -> GState
endGameCheck (Game s ai hist from)
  | null ai && isChecked s =
    GameOver s (show (nextPlayer (turn s)) ++ " wins!")
  | null ai =
    GameOver s "Draw!"
endGameCheck s = s

game0 :: State -> GState
game0 state = Game state (doAI state) "" Nothing
{-# NOINLINE game0 #-}

handleEvent :: Event -> GState -> GState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) gstate@(Game s ai hist from)
  = case from of Nothing ->
                   if pieceAtOwnedBy (board s) (turn s) cpos then
                   Game s ai hist (Just cpos) else
                   gstate
                 Just fromPos ->
                   case findByMove ai (Move fromPos cpos) of
                     Just tree -> aiDoMove $ execMove gstate tree
                     Nothing -> Game s ai hist Nothing
  where cpos = convertPosition (turn s)
          (floor ((x + fwindowSize / 2) / pieceScale),
           floor ((y + fwindowSize / 2) / pieceScale))

handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) PickPlayer =
  if x < 0 then game0 state0 else aiDoMove (game0 state0)
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
    PickPlayer --(Game state0 (doAI state0) "" Nothing)
    (return . displayGState sprites)
    (\e s -> return $ handleEvent e s)
    (\_ -> return ())
