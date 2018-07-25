module Drawing where

-- vty
import qualified Graphics.Vty.Image      as Vty
import qualified Graphics.Vty.Attributes as Vty

import Coord

data Drawing a
  = Image Vty.Image
  | Drawing a :||| Drawing a
  | Drawing a :--- Drawing a
  | Region a (Drawing a)

infixr 4 :---
infixr 5 :|||

drawingToImage :: Drawing a -> Vty.Image
drawingToImage d =
  case d of
    Image img  -> img
    x :||| y   -> drawingToImage x Vty.<|> drawingToImage y
    x :--- y   -> drawingToImage x Vty.<-> drawingToImage y
    Region _ x -> drawingToImage x

string :: Vty.Attr -> String -> Drawing a
string attr str = Image (Vty.string attr str)

char :: Vty.Attr -> Char -> Drawing a
char attr str = Image (Vty.char attr str)

emptyImage :: Drawing a
emptyImage = Image Vty.emptyImage

vertCat :: [Drawing a] -> Drawing a
vertCat = foldr (:---) emptyImage

horizCat :: [Drawing a] -> Drawing a
horizCat = foldr (:|||) emptyImage

coordRegions :: Coord -> Drawing a -> [a]
coordRegions c d = let (_,_,rs) = go c d in rs
  where
    go :: Coord -> Drawing a -> (Int, Int, [a])

    go (C x y) (Image i) = (Vty.imageWidth i, Vty.imageHeight i, [])

    go c@(C x y) (Region r i) =
      case go c i of
        (w,h,rs) -> (w,h,if x < w && y < h then r:rs else [])

    go c@(C x y) (a :||| b) =
      case go c a of
        (w_a,h_a,rs_a) ->
          case go (C (x - w_a) y) b of
            (w_b,h_b,rs_b) -> (w_a + w_b, max h_a h_b, if x < w_a then rs_a else rs_b)

    go c@(C x y) (a :--- b) =
      case go c a of
        (w_a,h_a,rs_a) ->
          case go (C x (y - h_a)) b of
            (w_b,h_b,rs_b) -> (max w_a w_b, h_a + h_b, if y < h_a then rs_a else rs_b)
