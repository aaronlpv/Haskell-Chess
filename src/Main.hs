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

windowSize :: Int
windowSize = 856

fwindowSize :: Float
fwindowSize = fromIntegral windowSize

unit :: Int
unit = 213

funit :: Float
funit = fromIntegral unit

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

displayGState :: [[Picture]] -> GState -> Picture
displayGState sprites (Game s _ _ _) = displayState sprites s
displayGState sprites PickPlayer =
  Pictures
  [Translate (-fwindowSize/4.5) (fwindowSize/5) $ Text "Chess",
   Scale 0.5 0.5 $ Translate (-fwindowSize/2.15) (-fwindowSize/6) $ Text "Pick a side",
   Translate (-fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ spriteForPiece sprites (Piece White King),
   Translate (fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ spriteForPiece sprites (Piece Black King)]
displayGState sprites (GameOver state msg) =
  Pictures
  [displayState sprites state,
   Color (greyN 0.3) (rectangleSolid fwindowSize 100),
   Translate (-180) (-25) $ Scale 0.5 0.5 $ Color white $ Text msg]

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
  | null ai && isChecked s (turn s) =
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
  where cpos = convertPosition (turn s) (floor ((x + fwindowSize / 2) / funit * 2),
                                  floor ((y + fwindowSize / 2) / funit * 2))

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
    (Game state0 (doAI state0) "" Nothing)
    (return . displayGState sprites)
    (\e s -> return $ handleEvent e s)
    (\_ -> return ())
