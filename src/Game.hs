module Game where

import Data.Bits
import Data.Word

-- | Pieces are represented as a number in the range @0@ to @15@.
-- These piece numbers can be represented in 4 bits with each
-- bit corresponding to one of the 4 attributes of a piece.
--
-- In the physical game these attributes are height, color, shape, solid.
newtype Piece = Piece { pieceId :: Int }
  deriving (Eq, Show)

-- | Positions on the board are represented by a number in the range
-- @0@ to @15@ laid out as follows (using hexadecimal digits):
--
-- @
-- 0123
-- 4567
-- 89ab
-- cdef
-- @
newtype Posn = Posn { posnId :: Int }
  deriving (Eq, Show)


-- | The Quarto board is represented by two 64-bit words.
-- Each of these words is divided into 16 groups of 4 bits.
-- 16 is a fortunate number of groups as this is the number of
-- pieces and positions in the game.
--
-- In the first word we track which pieces and which positions
-- have been used. The index of each 4-bit group corresponds to
-- a piece number and a position number.
--
-- Group N:
-- bit 0: Position N is occupied
-- bit 1: Piece N is used
-- bit 2: unused
-- bit 3: unused
--
-- In the second word each group of 4-bits is set to the piece number
-- of the piece stored at the position matching the group number.
-- If no piece has been placed at a position all attributes are set
-- to @0@.
--
-- Group N:
-- bit 0: attribute 0 of piece at position N
-- bit 1: attribute 1 of piece at position N
-- bit 2: attribute 2 of piece at position N
-- bit 3: attribute 3 of piece at position N
data Quarto = Quarto
  {-# UNPACK #-}!Word64 -- pieces placed and available
  {-# UNPACK #-}!Word64 -- attributes
  deriving (Eq, Ord, Show)

-- BEGIN BIT FIDDLING -------------------------------------------------

initialQuarto :: Quarto
initialQuarto = Quarto 0 0

positionCovered :: Quarto -> Posn -> Bool
positionCovered (Quarto x _) (Posn i) = testBit x (i*4)
{-# INLINE positionCovered #-}

pieceUsed :: Quarto -> Piece -> Bool
pieceUsed (Quarto x _) (Piece i) = testBit x (i*4+1)
{-# INLINE pieceUsed #-}

pieceAt :: Quarto -> Posn -> Maybe Piece
pieceAt q@(Quarto _ y) posn@(Posn p)
  | positionCovered q posn = Just $! Piece (fromIntegral (0xf .&. y`shiftR` (p*4)))
  | otherwise              = Nothing

setPieceAt ::
  Piece {- ^ piece    -} ->
  Posn  {- ^ position -} ->
  Quarto -> Quarto
setPieceAt (Piece piece) (Posn posn) (Quarto x y) = Quarto x' y'
  where
    x' = x `setBit` (posn *4  )
           `setBit` (piece*4+1)
    y' = y .|. fromIntegral piece`shiftL`(posn*4)

-- END OF BIT FIDDLING -------------------------------------------------

possibleMoves :: Quarto -> [(Piece, Posn, Quarto)]
possibleMoves q =
  [ q' `seq` (piece, posn, q')
     | piece <- Piece <$> [0 .. 0xf], not (pieceUsed q piece)
     , posn  <- Posn  <$> [0 .. 0xf], not (positionCovered q posn)
     , let q' = setPieceAt piece posn q
     ]


-- | Predicate for testing if the chosen bits are all set in the search space.
testAllSet ::
  Word64 {- ^ search space                        -} ->
  Word64 {- ^ chosen bits                         -} ->
  Bool   {- ^ all chosen bits set in search space -}
testAllSet x y = x.&.y == y

-- | Predicate for testing if the chosen bits are all equal in the search space.
testAllEqual ::
  Word64 {- ^ search space                        -} ->
  Word64 {- ^ chosen bits                         -} ->
  Bool   {- ^ all chosen bits equal in search space -}
testAllEqual x y = let z = x.&.y in z == y || z == 0

-- | Determine if the given row, column, or diagonal is in
-- a winning configuration on the given board.
matchFour :: Quarto -> Four -> Bool
matchFour (Quarto x y) (Four z) = allPlaced && attrMatch
  where
    allPlaced = testAllSet x z
    attrMatch = testAllEqual y (z `shiftL` 0) ||
                testAllEqual y (z `shiftL` 1) ||
                testAllEqual y (z `shiftL` 2) ||
                testAllEqual y (z `shiftL` 3)

-- | Predicate to determine if placing the piece at the given
-- position resulted in a win.
checkWinAt :: Posn -> Quarto -> Bool
checkWinAt i q = any (matchFour q) (candidateFours i)

-- | 'Four' values correspond to bit masks of 4 bits corresponding
-- to rows, columns, and diagonals on the game board. These values
-- make it easy to quickly test placement and common attributes
-- on a board using bit-manipulation.
newtype Four = Four Word64

row0, row1, row2, row3, col0, col1, col2, col3, dia0, dia1 :: Four
--            position:
--            fedcba9876543210
row0 = Four 0x0000000000001111
row1 = Four 0x0000000011110000
row2 = Four 0x0000111100000000
row3 = Four 0x1111000000000000
col0 = Four 0x0001000100010001
col1 = Four 0x0010001000100010
col2 = Four 0x0100010001000100
col3 = Four 0x1000100010001000
dia0 = Four 0x1000010000100001
dia1 = Four 0x0001001001001000

-- | Find the masks corresponding to the rows, columns, and diagonals
-- that contain a particular position.
candidateFours :: Posn -> [Four]
candidateFours (Posn i) =
  case i of
    0x0 -> [row0, col0, dia0]
    0x1 -> [row0, col1      ]
    0x2 -> [row0, col2      ]
    0x3 -> [row0, col3, dia1]
    0x4 -> [row1, col0      ]
    0x5 -> [row1, col1, dia0]
    0x6 -> [row1, col2, dia1]
    0x7 -> [row1, col3      ]
    0x8 -> [row2, col0      ]
    0x9 -> [row2, col1, dia1]
    0xa -> [row2, col2, dia0]
    0xb -> [row2, col3      ]
    0xc -> [row3, col0, dia1]
    0xd -> [row3, col1      ]
    0xe -> [row3, col2      ]
    0xf -> [row3, col3, dia0]
    _   -> error "candidateFours: invalid position"


-- | Result of searching the game tree with 'bestOutcome'
data Outcome
  = Lose                -- All moves lead to loss
  | Draw [(Piece,Posn)] -- Moves that lead to a draw
  | Win !Piece !Posn    -- Move that leads to a win
  deriving (Show)

-- | Consider sequences of moves up to the given depth. Determines the
-- best possible outcome that the current player can guarantee within
-- that depth of look-ahead.
bestOutcome ::
  Int     {- ^ depth          -} ->
  Quarto  {- ^ starting state -} ->
  Outcome {- ^ best outcome   -}
bestOutcome depth start = bestOutcome' depth (possibleMoves start)

bestOutcome' ::
  Int                     {- ^ fuel             -} ->
  [(Piece, Posn, Quarto)] {- ^ possible futures -} ->
  Outcome                 {- ^ best outcome     -}
bestOutcome' depth xs0
  | depth == 0 || null xs0 = Draw []
  | otherwise              = go [] xs0
  where
    go draws [] | null draws = Lose -- no draws means we skipped over a Win
                | otherwise  = Draw draws
    go draws ((piece,posn,q):xs)
      | checkWinAt posn q = Win piece posn
      | otherwise =
          case bestOutcome (depth-1) q of
            Lose{} -> Win piece posn -- opponent's loss is our win
            Win {} -> go draws xs    -- opponent's win is our loss
            Draw{} -> go ((piece,posn):draws) xs

