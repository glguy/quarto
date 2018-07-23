module Main (main) where

-- base
import Control.Concurrent.Async (Async, async, cancel, waitCatchSTM)
import Control.Exception        (bracket, evaluate)
import Data.Char                (digitToInt, intToDigit, isHexDigit)
import Data.Foldable            (asum, traverse_)
import Data.Maybe               (isJust)

-- random
import System.Random (randomRIO)

-- stm
import Control.Concurrent.STM
         (TChan, newTChanIO, readTChan, atomically, retry, writeTChan)

-- vty
import Graphics.Vty

-- local
import Coord
import BoxDrawing
import Game

-- | State of application
data App = App
  { appVty    :: Vty                   -- ^ terminal handle
  , appQuarto :: Quarto                -- ^ game board
  , appMode   :: Modality              -- ^ application input mode
  , appSearch :: Maybe (Int, Result)   -- ^ most recent search result
  , appHint   :: Maybe (Posn, Piece)   -- ^ most recent move hint
  , appAsync  :: Maybe (Async ())      -- ^ handle to search thread
  , appChannel :: TChan (Int, Outcome) -- ^ results from search thread
  }

-- | The result of a search can indicate: Win, Lose, and Draw
data Result = W | L | D

data Modality
  = NoInput
  | PosnInput Posn
  | PosnPieceInput Posn Piece
  | GameOver
  deriving Show

isGameOver :: Modality -> Bool
isGameOver GameOver = True
isGameOver _        = False

main :: IO ()
main =
  newTChanIO >>= \tchan ->
  bracket (mkVty mempty) shutdown $ \vty ->
  main' =<< doAI App
    { appVty    = vty
    , appQuarto = initialQuarto
    , appMode   = NoInput
    , appSearch = Nothing
    , appHint   = Nothing
    , appAsync  = Nothing
    , appChannel = tchan }

doDraw :: App -> IO ()
doDraw app = update (appVty app) (picForImage (drawApp app))

data AppEvent
  = VtyEvent Event
  | HintEvent (Int,Outcome)
  | AsyncFinished

getEvent :: App -> IO AppEvent
getEvent app =
  atomically $ asum
    [ VtyEvent      <$> readTChan (_eventChannel (inputIface (appVty app)))
    , HintEvent     <$> readTChan (appChannel app)
    , AsyncFinished <$  maybe retry waitCatchSTM (appAsync app) ]

main' :: App -> IO ()
main' app =
  do doDraw app
     ev <- getEvent app
     case ev of
       AsyncFinished -> main' app { appAsync = Nothing }
       VtyEvent vtyEvent -> doVtyEvent app vtyEvent
       HintEvent hint ->
         case hint of
           (depth, Draw []) ->
             main' app { appHint = Nothing
                       , appSearch = Just (depth, D) }
           (depth, Draw xs) ->
             do i <- randomRIO (0, length xs - 1)
                let (piece,posn) = xs !! i
                main' app { appHint = Just (posn, piece)
                          , appSearch = Just (depth, D) }
           (depth, Win piece posn) ->
             main' app { appHint = Just (posn, piece)
                       , appSearch = Just (depth, W) }
           (depth, Lose) ->
               main' app { appSearch = Just (depth, L) }


doAI :: App -> IO App
doAI app =
  do traverse_ cancel (appAsync app)
     if isGameOver (appMode app)
       then return app
       else do thread <- ai (appChannel app) (appQuarto app)
               return $! app { appAsync = Just thread }

doVtyEvent :: App -> Event -> IO ()
doVtyEvent app ev =
     case (ev, appMode app) of

       -- Quit
       (EvKey KEsc [], _) -> return () -- exit event loop

       -- Cancel position choice
       (EvKey KBS [], PosnInput _) ->
         main' app { appMode = NoInput }

       -- Cancel piece choice
       (EvKey KBS [], PosnPieceInput posn _) ->
         main' app { appMode = PosnInput posn }

       -- Stop AI
       (EvKey (KChar 'x') [], _) ->
         do traverse_ cancel (appAsync app)
            main' app { appAsync = Nothing }

       -- Use hint as input
       (EvKey (KChar 'h') [], mode)
         | not (isGameOver mode)
         , Just (posn, piece) <- appHint app ->
           main' app { appMode = PosnPieceInput posn piece }

       -- Restart
       (EvKey (KChar 'n') [], _) ->
         do traverse_ cancel (appAsync app)
            main' =<< doAI app { appAsync  = Nothing
                               , appHint   = Nothing
                               , appQuarto = initialQuarto
                               , appMode   = NoInput }

       -- 1. Choose a position
       (EvKey (KChar c) [], NoInput)
         | isHexDigit c
         , let posn = Posn (digitToInt c)
         , not (positionCovered (appQuarto app) posn) ->
         main' app { appMode = PosnInput posn }

       -- 2. Choose a piece
       (EvKey (KChar c) [], PosnInput posn)
         | isHexDigit c
         , let piece = Piece (digitToInt c)
         , not (pieceUsed (appQuarto app) piece) ->
         main' app { appMode = PosnPieceInput posn piece }

       -- 3. Confirm placement
       (EvKey KEnter [], PosnPieceInput posn piece) ->
         do let quarto   = setPieceAt piece posn (appQuarto app)
                finished = checkWinAt posn quarto
            main' =<< doAI app { appMode     = if finished then GameOver else NoInput
                               , appQuarto   = quarto
                               , appHint     = Nothing }

       -- Ignore everything else
       _ -> main' app

drawApp :: App -> Image
drawApp app =
  pieceKey   (appMode app) (appQuarto app) <->
  drawQuarto (appMode app) (appQuarto app) <->
  drawMode   (appMode app)                 <->
  if isGameOver (appMode app)
    then emptyImage
    else renderHint (isJust (appAsync app)) (appSearch app) (appHint app)

drawMode :: Modality -> Image
drawMode NoInput          = string defAttr "<Select position>"
drawMode PosnInput     {} = string defAttr "<Select piece>"
drawMode PosnPieceInput{} = string defAttr "<Press ENTER>"
drawMode GameOver         = string defAttr "Game Over (new game: n)"

drawQuarto :: Modality -> Quarto -> Image
drawQuarto inp q = renderGrid 4 4 edge cell
  where
    selPosn
      = fmap (\x -> posnId x `divMod` 4)
      $ case inp of
          PosnInput      p   -> Just p
          PosnPieceInput p _ -> Just p
          _                  -> Nothing

    selRow = fst <$> selPosn
    selCol = snd <$> selPosn

    -- boundary of selection
    edge (C col row) o
      | selRow == Just row || o == Horiz && selRow == Just (row-1)
      , selCol == Just col || o == Vert  && selCol == Just (col-1) = Just Heavy
      | otherwise = Just Thin

    cell (C col row) =
      let posn = Posn (row * 4 + col) in
      case inp of
        PosnInput p | p == posn ->
          string (defAttr `withForeColor` magenta) "??"
        PosnPieceInput p piece | p == posn ->
          pieceGlyph piece
        _ ->
          case pieceAt q posn of
            Nothing    -> string defAttr (intToDigit (posnId posn) : " ")
            Just piece -> pieceGlyph piece

-- | Draw the available pieces and corresponding piece IDs
pieceKey :: Modality -> Quarto -> Image
pieceKey inp q
  = horizCat
  $ [ string defAttr (intToDigit i : "  ") <->
      pieceGlyph piece
    | i <- [0..0xf]
    , let piece = Piece i
    , not (pieceUsed q piece), Just piece /= picked ]
  where
    picked = case inp of
                PosnPieceInput _ p -> Just p
                _                  -> Nothing

renderHint :: Bool -> Maybe (Int, Result) -> Maybe (Posn, Piece) -> Image
renderHint running search hint =
  horizCat [searchPart, movePart, runningPart]

  where
    runningPart = if running then string defAttr " [thinking]" else emptyImage

    searchPart =
      case search of
        Nothing         -> string defAttr "No search result "
        Just (depth, W) -> string defAttr ("Win in " ++ show depth)
        Just (depth, D) -> string defAttr ("Draw in " ++ show depth)
        Just (depth, L) -> string defAttr ("Lose in " ++ show depth)

    movePart =
      case hint of
        Nothing -> string defAttr ": no suggestion"
        Just (posn, piece) ->
          string defAttr ": position " <|>
          drawPosn posn                <|>
          string defAttr " and piece " <|>
          drawPiece piece


drawPosn :: Posn -> Image
drawPosn p =
  char (defAttr `withStyle` bold) (intToDigit (posnId p))

drawPiece :: Piece -> Image
drawPiece p =
  char (defAttr `withStyle` bold) (intToDigit (pieceId p)) <|>
  string defAttr " ["                                      <|>
  pieceGlyph p                                             <|>
  char defAttr ']'

pieceGlyph :: Piece -> Image
pieceGlyph (Piece p) = string (defAttr `withForeColor` color `withStyle` bold) str
  where
    color = if p < 8 then green else red
    str =
      case p `mod` 8 of
        0 -> "● "
        1 -> "○ "
        2 -> "■ "
        3 -> "□ "
        4 -> "●●"
        5 -> "○○"
        6 -> "■■"
        7 -> "□□"
        _ -> error "renderPiece: impossible"

-- | Iterative-deepening depth-first search of the game tree.
-- This action will run up to the given amount of time printing
-- outcomes of searches at increasing depths until time expires
-- or a conclusive outcome is determined.
ai ::
  TChan (Int,Outcome) {- ^ output channel -} ->
  Quarto {- ^ game configuration -} ->
  IO (Async ())
ai tchan q = async (loop 1)
  where
    maxDepth = length (filter (not . positionCovered q . Posn) [0..0xf])

    loop depth =
      do let outcome = bestOutcome depth q
         evaluate outcome
         atomically (writeTChan tchan (depth, outcome))
         case outcome of
           Draw (_:_) | depth < maxDepth -> loop (depth+1)
           _                             -> return ()
