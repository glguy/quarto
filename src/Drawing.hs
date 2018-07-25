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
coordRegions (C x0 y0) d = case go x0 y0 d of (_,_,rs) -> rs
  where
    go _ _ (Image i) = (Vty.imageWidth i, Vty.imageHeight i, [])

    go x y (Region r a) =
      case go x y a of
        (w,h,rs) -> (w,h,if x < w && y < h then r:rs else [])

    go x y (a :||| b) =
      case go x y a of
        (w_a,h_a,rs_a) ->
          case go (x - w_a) y b of
            (w_b,h_b,rs_b) ->
              (w_a + w_b, max h_a h_b, if x < w_a then rs_a else rs_b)

    go x y (a :--- b) =
      case go x y a of
        (w_a,h_a,rs_a) ->
          case go x (y - h_a) b of
            (w_b,h_b,rs_b) ->
              (max w_a w_b, h_a + h_b, if y < h_a then rs_a else rs_b)
