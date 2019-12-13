import Data.Maybe (fromJust)
import Codec.BMP
import Control.Monad
import Data.Either (fromRight)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Board
import Position
import State
import Types
import Chess (step)
import Move

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

convertPosition :: Player -> Position -> Position
convertPosition White p = p
convertPosition Black (x,y) = (7-x, 7-y)

displayState :: [[Picture]] -> GState -> Picture
displayState sprites (Game (State b t _ _ _) _) =
  Color black $
  Scale (funit / 2) (funit / 2) $
  Translate (-3.5) (-3.5) (pictures $ grid ++ pieces)
  where
    pieces =
      map
        (\(pos, Piece p k) ->
           case convertPosition t pos of (x,y) ->
                                             Translate
                                             (fromIntegral x)
                                             (fromIntegral y)
                                             ((sprites !! fromEnum p) !! fromEnum k))
        (allPieces b)
    grid =
      [ Translate (fromIntegral x) (fromIntegral y) (rectangleSolid 1 1)
      | x <- [0,1 .. 7]
      , y <- [0,1 .. 7]
      , (odd x == odd y) == (t == White)
      ]
displayState sprites PickPlayer =
  Pictures
  [Translate (-fwindowSize/4.5) (fwindowSize/5) $ Text "Chess",
   Scale 0.5 0.5 $ Translate (-fwindowSize/2.15) (-fwindowSize/6) $ Text "Pick a side",
   Translate (-fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ (sprites !! fromEnum White) !! fromEnum King,
   Translate (fwindowSize/5) (-fwindowSize/4) $ Scale (funit/2) (funit/2) $ (sprites !! fromEnum Black) !! fromEnum King]
displayState _ _ = error "not implemented" -- FIXME

handleEvent :: Event -> GState -> GState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) state@(Game s@(State b t _ _ _) from)
  = case from of Nothing -> if pieceAtOwnedBy b t cpos then Game s (Just cpos) else state
                 Just fromPos ->
                   let game = fromJust $ snd $ step s (Move fromPos cpos) in
                     Game game Nothing
  where cpos = convertPosition t (floor ((x + fwindowSize / 2) / funit * 2),
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
