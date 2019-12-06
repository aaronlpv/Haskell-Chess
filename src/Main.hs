import Board (allPieces)
import Codec.BMP
import Data.Either (fromRight)
import Graphics.Gloss
import Position (Position)
import State (State(..), state0)
import Types (Piece(..), PieceType(..), Player(..))

unit :: Int
unit = 213

funit :: Float
funit = fromIntegral unit

loadSprites :: BitmapData -> [[Picture]]
loadSprites bmp = [loadSp 0, loadSp unit]
  where
    loadSp y =
      [ Scale (1 / funit) (1 / funit) $
      bitmapSection (Rectangle (x * unit, y) (unit, unit)) bmp
      | x <- [0,1 .. 7]
      ]

displayState :: State -> [[Picture]] -> Picture
displayState (State b t _ _ _) sprites = pictures $ grid ++ pieces
  where
    pieces =
      map
        (\((x, y), Piece p k) ->
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

main = do
  bmp <-
    readBMP
      "/home/aaron/Documents/univ/1MA/Func/assignments/chess/test/sprites2.bmp"
  let sprites =
        loadSprites $
        bitmapDataOfBMP (fromRight (error "Sprites not found!") bmp)
  display
    (InWindow "Chess" (856, 856) (10, 10))
    white
    (Color black $
     Scale (funit / 2) (funit / 2) $
     Translate (-3.5) (-3.5) (displayState state0 sprites))
