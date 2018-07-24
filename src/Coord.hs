module Coord
  (
  -- * Coordinates
    Coord(C), up, down, left, right
  ) where

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

up, down, left, right :: Coord -> Coord
up    (C x y) = C x (y-1)
down  (C x y) = C x (y+1)
left  (C x y) = C (x-1) y
right (C x y) = C (x+1) y
