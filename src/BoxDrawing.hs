{-|
Module      : BoxDrawing
Description : Tools for rendering boxes on consoles
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module BoxDrawing
  ( Weight(Thin, Heavy, Double)
  , Orient(Horiz, Vert)
  , boxChar
  , renderGrid
  ) where

-- base
import Control.Monad (guard)
import Data.List     (transpose)
import Data.Maybe    (fromMaybe)

-- vty
import Graphics.Vty.Attributes

import Coord
import Drawing

-- | Indicates line style
data Weight
  = Thin   -- ^ thin/normal lines
  | Heavy  -- ^ heavy lines
  | Double -- ^ double lines
  deriving (Eq, Ord, Read, Show)

-- | Used to indicate the top (horizontal) or left (vertical) edge
-- of a cell at a particular coordinate.
data Orient
  = Horiz -- ^ horizontal
  | Vert  -- ^ vertical
  deriving (Eq, Ord, Read, Show)

-- | Returns box drawing character corresponding to the given
-- weights eminating out from the center of the cell.
--
-- Straight lines will use the same weight in the up/down or left/right
-- directions.
--
-- Some combinations of box drawing characters are unavailable when
-- using double-thickness lines.
--
-- @
--  |       up
-- -+-  left+right
--  |      down
-- @
boxChar ::
  Maybe Weight {- ^ up                    -} ->
  Maybe Weight {- ^ down                  -} ->
  Maybe Weight {- ^ left                  -} ->
  Maybe Weight {- ^ right                 -} ->
  Maybe Char   {- ^ box drawing character -}
boxChar Nothing       Nothing       Nothing       Nothing       = Just ' '
boxChar (Just Thin  ) Nothing       Nothing       Nothing       = Just '╵'
boxChar (Just Heavy ) Nothing       Nothing       Nothing       = Just '╹'
boxChar Nothing       (Just Thin  ) Nothing       Nothing       = Just '╷'
boxChar Nothing       (Just Heavy ) Nothing       Nothing       = Just '╻'
boxChar Nothing       Nothing       (Just Thin  ) Nothing       = Just '╴'
boxChar Nothing       Nothing       (Just Heavy ) Nothing       = Just '╸'
boxChar Nothing       Nothing       Nothing       (Just Thin  ) = Just '╶'
boxChar Nothing       Nothing       Nothing       (Just Heavy ) = Just '╺'
boxChar (Just Thin  ) (Just Thin  ) Nothing       Nothing       = Just '│'
boxChar (Just Heavy ) (Just Heavy ) Nothing       Nothing       = Just '┃'
boxChar (Just Double) (Just Double) Nothing       Nothing       = Just '║'
boxChar (Just Thin  ) (Just Heavy ) Nothing       Nothing       = Just '╽'
boxChar (Just Heavy ) (Just Thin  ) Nothing       Nothing       = Just '╿'
boxChar (Just Thin  ) Nothing       (Just Thin  ) Nothing       = Just '┘'
boxChar (Just Heavy ) Nothing       (Just Heavy ) Nothing       = Just '┛'
boxChar (Just Double) Nothing       (Just Double) Nothing       = Just '╝'
boxChar (Just Thin  ) Nothing       (Just Heavy ) Nothing       = Just '┙'
boxChar (Just Heavy ) Nothing       (Just Thin  ) Nothing       = Just '┚'
boxChar (Just Thin  ) Nothing       (Just Double) Nothing       = Just '╛'
boxChar (Just Double) Nothing       (Just Thin  ) Nothing       = Just '╜'
boxChar (Just Thin  ) Nothing       Nothing       (Just Thin  ) = Just '└'
boxChar (Just Heavy ) Nothing       Nothing       (Just Heavy ) = Just '┗'
boxChar (Just Double) Nothing       Nothing       (Just Double) = Just '╚'
boxChar (Just Thin  ) Nothing       Nothing       (Just Heavy ) = Just '┕'
boxChar (Just Heavy ) Nothing       Nothing       (Just Thin  ) = Just '┖'
boxChar (Just Thin  ) Nothing       Nothing       (Just Double) = Just '╘'
boxChar (Just Double) Nothing       Nothing       (Just Thin  ) = Just '╙'
boxChar Nothing       (Just Thin  ) (Just Thin  ) Nothing       = Just '┐'
boxChar Nothing       (Just Heavy ) (Just Heavy ) Nothing       = Just '┓'
boxChar Nothing       (Just Double) (Just Double) Nothing       = Just '╗'
boxChar Nothing       (Just Thin  ) (Just Heavy ) Nothing       = Just '┑'
boxChar Nothing       (Just Heavy ) (Just Thin  ) Nothing       = Just '┒'
boxChar Nothing       (Just Thin  ) (Just Double) Nothing       = Just '╕'
boxChar Nothing       (Just Double) (Just Thin  ) Nothing       = Just '╖'
boxChar Nothing       (Just Thin  ) Nothing       (Just Thin  ) = Just '┌'
boxChar Nothing       (Just Heavy ) Nothing       (Just Heavy ) = Just '┏'
boxChar Nothing       (Just Double) Nothing       (Just Double) = Just '╔'
boxChar Nothing       (Just Thin  ) Nothing       (Just Heavy ) = Just '┍'
boxChar Nothing       (Just Heavy ) Nothing       (Just Thin  ) = Just '┎'
boxChar Nothing       (Just Thin  ) Nothing       (Just Double) = Just '╒'
boxChar Nothing       (Just Double) Nothing       (Just Thin  ) = Just '╓'
boxChar Nothing       Nothing       (Just Thin  ) (Just Thin  ) = Just '─'
boxChar Nothing       Nothing       (Just Heavy ) (Just Heavy ) = Just '━'
boxChar Nothing       Nothing       (Just Thin  ) (Just Heavy ) = Just '╼'
boxChar Nothing       Nothing       (Just Heavy ) (Just Thin  ) = Just '╾'
boxChar Nothing       Nothing       (Just Double) (Just Double) = Just '═'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) Nothing       = Just '┤'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) Nothing       = Just '┫'
boxChar (Just Double) (Just Double) (Just Double) Nothing       = Just '╣'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) Nothing       = Just '┦'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) Nothing       = Just '┧'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) Nothing       = Just '┥'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) Nothing       = Just '┪'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) Nothing       = Just '┩'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) Nothing       = Just '┨'
boxChar (Just Thin  ) (Just Thin  ) (Just Double) Nothing       = Just '╡'
boxChar (Just Double) (Just Double) (Just Thin  ) Nothing       = Just '╢'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Thin  ) = Just '├'
boxChar (Just Heavy ) (Just Heavy ) Nothing       (Just Heavy ) = Just '┣'
boxChar (Just Double) (Just Double) Nothing       (Just Double) = Just '╠'
boxChar (Just Heavy ) (Just Thin  ) Nothing       (Just Thin  ) = Just '┞'
boxChar (Just Thin  ) (Just Heavy ) Nothing       (Just Thin  ) = Just '┟'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Heavy ) = Just '┝'
boxChar (Just Thin  ) (Just Heavy ) Nothing       (Just Heavy ) = Just '┢'
boxChar (Just Heavy ) (Just Thin  ) Nothing       (Just Heavy ) = Just '┡'
boxChar (Just Heavy ) (Just Heavy ) Nothing       (Just Thin  ) = Just '┠'
boxChar (Just Thin  ) (Just Thin  ) Nothing       (Just Double) = Just '╞'
boxChar (Just Double) (Just Double) Nothing       (Just Thin  ) = Just '╟'
boxChar (Just Thin  ) Nothing       (Just Thin  ) (Just Thin  ) = Just '┴'
boxChar (Just Heavy ) Nothing       (Just Heavy ) (Just Heavy ) = Just '┻'
boxChar (Just Double) Nothing       (Just Double) (Just Double) = Just '╩'
boxChar (Just Heavy ) Nothing       (Just Thin  ) (Just Thin  ) = Just '┸'
boxChar (Just Thin  ) Nothing       (Just Heavy ) (Just Thin  ) = Just '┵'
boxChar (Just Thin  ) Nothing       (Just Thin  ) (Just Heavy ) = Just '┶'
boxChar (Just Thin  ) Nothing       (Just Heavy ) (Just Heavy ) = Just '┷'
boxChar (Just Heavy ) Nothing       (Just Thin  ) (Just Heavy ) = Just '┺'
boxChar (Just Heavy ) Nothing       (Just Heavy ) (Just Thin  ) = Just '┹'
boxChar (Just Thin  ) Nothing       (Just Double) (Just Double) = Just '╧'
boxChar (Just Double) Nothing       (Just Thin  ) (Just Thin  ) = Just '╨'
boxChar Nothing       (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '┬'
boxChar Nothing       (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '┳'
boxChar Nothing       (Just Double) (Just Double) (Just Double) = Just '╦'
boxChar Nothing       (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '┰'
boxChar Nothing       (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '┭'
boxChar Nothing       (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '┮'
boxChar Nothing       (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '┯'
boxChar Nothing       (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '┲'
boxChar Nothing       (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '┱'
boxChar Nothing       (Just Thin  ) (Just Double) (Just Double) = Just '╤'
boxChar Nothing       (Just Double) (Just Thin  ) (Just Thin  ) = Just '╥'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '┼'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) (Just Thin  ) = Just '╀'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '╁'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '┽'
boxChar (Just Thin  ) (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '┾'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) (Just Thin  ) = Just '╂'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) (Just Thin  ) = Just '╃'
boxChar (Just Heavy ) (Just Thin  ) (Just Thin  ) (Just Heavy ) = Just '╄'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '╅'
boxChar (Just Thin  ) (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '╆'
boxChar (Just Thin  ) (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '┿'
boxChar (Just Thin  ) (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '╈'
boxChar (Just Heavy ) (Just Thin  ) (Just Heavy ) (Just Heavy ) = Just '╇'
boxChar (Just Heavy ) (Just Heavy ) (Just Thin  ) (Just Heavy ) = Just '╊'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) (Just Thin  ) = Just '╉'
boxChar (Just Heavy ) (Just Heavy ) (Just Heavy ) (Just Heavy ) = Just '╋'
boxChar (Just Double) (Just Double) (Just Double) (Just Double) = Just '╬'
boxChar (Just Thin  ) (Just Thin  ) (Just Double) (Just Double) = Just '╪'
boxChar (Just Double) (Just Double) (Just Thin  ) (Just Thin  ) = Just '╫'
boxChar _             _             _             _             = Nothing


-- | Render a grid given a function for determining the weight of edges
-- and the characters in each cell.
--
-- The edge function will be called for the top and left edges of each
-- cell coordinate.
--
-- Cells indexes are zero-based. The origin as in the top-left of the
-- output.
--
-- @
-- putStr (renderGrid 3 3 1
--            (\(C x y) o -> if even x && even y
--                           || even x && o == Horiz
--                           || even y && o == Vert
--                           then Just Heavy
--                           else Just Thin)
--            (\(C x y) -> if even x && even y then '·' else ' '))
-- ┏━┱─┲━┓
-- ┃·┃ ┃·┃
-- ┡━╃─╄━┩
-- │ │ │ │
-- ┢━╅─╆━┪
-- ┃·┃ ┃·┃
-- ┗━┹─┺━┛
-- @
renderGrid ::
  Int                               {- ^ width      -} ->
  Int                               {- ^ height     -} ->
  Int                               {- ^ cell width -} ->
  (Coord -> Orient -> Maybe Weight) {- ^ edge logic -} ->
  (Coord -> Drawing a)              {- ^ cell logic -} ->
  Drawing a                         {- ^ grid lines -}
renderGrid w h i edge cell =
  vertCat [ horizCat [render1 x y | x <- [0..w] ] | y <- [0..h] ]
  where
    box u d l r = fromMaybe ' ' (boxChar u d l r)

    check p x = if p then x else emptyImage

    render1 x y = corner :||| check (x<w) edgeR
             :--- check (y<h) (edgeD :||| check (x<w) (cell c))
      where
        c  = C x y
        eR = guard (x<w) >> edge c        Horiz
        eD = guard (y<h) >> edge c        Vert
        eL = guard (0<x) >> edge (left c) Horiz
        eU = guard (0<y) >> edge (up   c) Vert

        corner = char defAttr (box eU eD eL eR)
        edgeD  = char defAttr (box eD eD Nothing Nothing)
        edgeR  = string defAttr (replicate i (box Nothing Nothing eR eR))
