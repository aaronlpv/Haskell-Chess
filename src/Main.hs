import Board (allPieces, Board (..), pieceAt)
import Data.Maybe (fromJust)
import Codec.BMP
import Control.Monad
import Data.Either (fromRight)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Position (Position)
import State (State(..), state0)
import Types (Piece(..), PieceType(..), Player(..))
import Chess (update)
import Move (Move (..))

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
           case (convertPosition t pos) of (x,y) ->
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
displayState _ _ = (error "not implemented") -- FIXME

handleEvent :: Event -> GState -> GState
handleEvent event state@(Game s@(State b t _ _ _) from)
  | EventKey (MouseButton LeftButton) Up _ pt@(x,y) <- event
  , Nothing <- from
  = Game s (Just (readPos x y))
  | EventKey (MouseButton LeftButton) Up _ pt@(x,y) <- event
  , Just pos <- from
  = let pos2 = (readPos x y)
        game = fromJust $ snd $ update s (Move pos pos2) (pieceAt (board s) pos) (pieceAt (board s) pos2) in
      Game game Nothing
  | otherwise = state
  where readPos x y = convertPosition t (floor ((x + fwindowSize / 2) / funit * 2),
                                         floor ((y + fwindowSize / 2) / funit * 2))
handleEvent _ s = s

debug :: GState -> IO ()
debug (Game s p) = putStrLn (show p)
debug _ = return ()

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
    gstate0
    (return . displayState sprites)
    (\e s -> do
        let ns = handleEvent e s
        debug ns
        return ns)
    (\_ -> return ())
