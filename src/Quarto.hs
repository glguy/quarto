module Main (main) where

-- base
import Control.Concurrent.Async (Async, async, cancel, waitCatchSTM)
import Control.Exception        (bracket, evaluate)
import Data.Char                (digitToInt, intToDigit, isHexDigit)
import Data.Foldable            (asum, traverse_)
import Data.List                (intersperse)
import Data.Maybe               (fromMaybe, isJust)

-- random
import System.Random (randomRIO)

-- stm
import Control.Concurrent.STM
         (TQueue, newTQueueIO, readTChan, readTQueue, atomically, retry, writeTQueue)

-- vty
import Graphics.Vty

-- local
import Coord
import BoxDrawing
import Game

-- | State of application
data App = App
  { appVty    :: Vty                    -- ^ terminal handle
  , appQuarto :: Quarto                 -- ^ game board
  , appMode   :: Modality               -- ^ application input mode
  , appSearch :: Maybe (Int, Result)    -- ^ most recent search result
  , appHint   :: Maybe (Posn, Piece)    -- ^ most recent move hint
  , appAsync  :: Maybe (Async ())       -- ^ handle to search thread
  , appChannel :: TQueue (Int, Outcome) -- ^ results from search thread
  }

-- | The result of a search can indicate: Win, Lose, and Draw
data Result = W | L | D

data Modality
  = NoInput
  | PosnInput Posn
  | PosnPieceInput Posn Piece
  | GameOver [Posn] -- list of positions to highlight
  deriving Show

isGameOver :: Modality -> Bool
isGameOver GameOver{} = True
isGameOver _          = False

activeAttr :: Attr
activeAttr = defAttr `withStyle` bold

inactiveAttr :: Attr
inactiveAttr = defAttr `withForeColor` Color240 224

main :: IO ()
main =
  newTQueueIO >>= \tchan ->
  bracket (mkVty mempty) shutdown $ \vty ->
  main' App
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
    , HintEvent     <$> readTQueue (appChannel app)
    , AsyncFinished <$  maybe retry waitCatchSTM (appAsync app) ]

main' :: App -> IO ()
main' app =
  do doDraw app
     ev <- getEvent app
     case ev of

       AsyncFinished -> main' app { appAsync = Nothing }

       VtyEvent (EvKey key []) -> doKeyEvent app key
       VtyEvent _              -> main' app

       HintEvent hint ->
         case hint of

           (depth, Draw []) ->
             main' app
               { appHint   = Nothing
               , appSearch = Just (depth, D) }

           (depth, Draw xs) ->
             do i <- randomRIO (0, length xs - 1)
                let (piece,posn) = xs !! i
                main' app
                  { appHint   = Just (posn, piece)
                  , appSearch = Just (depth, D) }

           (depth, Win piece posn) ->
             main' app
               { appHint   = Just (posn, piece)
               , appSearch = Just (depth, W) }

           (depth, Lose) ->
               main' app { appSearch = Just (depth, L) }


-- | Cancel any previous search thread and create a new one for the current
-- game state.
doAI :: App -> IO App
doAI app =
  do traverse_ cancel (appAsync app)
     if isGameOver (appMode app)
       then return app
       else do thread <- ai (appChannel app) (appQuarto app)
               return app { appAsync = Just thread }

doKeyEvent :: App -> Key -> IO ()
doKeyEvent app key =
     case (key, appMode app) of

       -- Quit
       (KEsc, _) -> return () -- exit event loop

       -- Cancel position choice
       (KBS, PosnInput _) ->
         main' app { appMode = NoInput }

       -- Cancel piece choice
       (KBS, PosnPieceInput posn _) ->
         main' app { appMode = PosnInput posn }

       -- Start AI
       (KChar '?', _) ->
         main' =<< doAI app

       -- Stop AI
       (KChar 'x', _) ->
         do traverse_ cancel (appAsync app)
            main' app { appAsync = Nothing }

       -- Use hint as input
       (KChar 'h', mode)
         | not (isGameOver mode)
         , Just (posn, piece) <- appHint app ->
           main' app { appMode = PosnPieceInput posn piece }

       -- Restart
       (KChar 'n', _) ->
         do traverse_ cancel (appAsync app)
            main' app
              { appAsync  = Nothing
              , appHint   = Nothing
              , appSearch = Nothing
              , appQuarto = initialQuarto
              , appMode   = NoInput }

       -- 1. Choose a position
       (KChar c, NoInput)
         | isHexDigit c
         , let posn = Posn (digitToInt c)
         , not (positionCovered (appQuarto app) posn) ->
         main' app { appMode = PosnInput posn }

       -- 2. Choose a piece
       (KChar c, PosnInput posn)
         | isHexDigit c
         , let piece = Piece (digitToInt c)
         , not (pieceUsed (appQuarto app) piece) ->
         main' app { appMode = PosnPieceInput posn piece }

       -- 3. Confirm placement
       (KEnter, PosnPieceInput posn piece) ->
         do let quarto   = setPieceAt piece posn (appQuarto app)
                newMode
                  | checkWinAt posn quarto =
                      GameOver (fromMaybe [] (winningPositions posn quarto))
                  | isBoardFull quarto = GameOver []
                  | otherwise = NoInput
            traverse_ cancel (appAsync app)
            main' app
              { appMode   = newMode
              , appQuarto = quarto
              , appHint   = Nothing
              , appSearch = Nothing
              , appAsync  = Nothing }

       -- Ignore everything else
       _ -> main' app

drawApp :: App -> Image
drawApp app =
  string defAttr " Game Board  -  Available Pieces" <->

  drawQuarto (appMode app) (appQuarto app) <|>
  pieceKey   (appMode app) (appQuarto app) <->

  drawCommands app <->

  if isGameOver (appMode app)
    then emptyImage
    else drawHint (appSearch app) (appHint app)

drawCommands :: App -> Image
drawCommands app =
  modePart <->
  horizCat (intersperse (char defAttr ' ') parts)
  where
    parts = quitPart : newgamePart : searchPart ++ hintPart

    part x y = string activeAttr x <|> string defAttr (':':y)

    quitPart = part "ESC" "quit"

    newgamePart = part "n" "new-game"

    hintPart =
      case appHint app of
        Just{}  -> [part "h" "use-hint"]
        Nothing -> []

    searchPart =
      case appAsync app of
        Just{}                             -> [part "x" "stop-search"]
        Nothing | isGameOver (appMode app) -> []
                | otherwise                -> [part "?" "start-search"]

    modePart =
      case appMode app of
        NoInput          -> part "0-f" "position"
        PosnInput     {} -> part "0-f" "piece"
        PosnPieceInput{} -> part "ENTER" "confirm"
        GameOver      {} -> string defAttr "Game Over"


drawQuarto :: Modality -> Quarto -> Image
drawQuarto inp q = renderGrid 4 4 2 edge cell
  where
    posnAttr =
      case inp of
        NoInput -> activeAttr
        _       -> inactiveAttr

    selected
      = map (\x -> C (x`mod`4) (x`div`4))
      $ map posnId
      $ case inp of
          PosnInput      p   -> [p]
          PosnPieceInput p _ -> [p]
          GameOver ps        -> ps
          NoInput            -> []

    -- boundary of selection
    edge c Horiz = edge1 c up
    edge c Vert  = edge1 c left
    edge1 c f
      | (c `elem` selected) /= (f c `elem` selected) = Just Heavy
      | otherwise                                    = Just Thin

    cell (C col row) =
      let posn = Posn (row * 4 + col) in
      case inp of
        PosnInput p | p == posn ->
          string (defAttr `withForeColor` magenta) "??"
        PosnPieceInput p piece | p == posn ->
          pieceGlyph piece
        _ ->
          case pieceAt q posn of
            Nothing    -> string posnAttr (intToDigit (posnId posn) : " ")
            Just piece -> pieceGlyph piece

-- | Draw the available pieces and corresponding piece IDs
pieceKey :: Modality -> Quarto -> Image
pieceKey inp q = renderGrid 4 4 4 (\_ _ -> Nothing) $ \(C col row) ->
  let piece = Piece (row*4+col) in
  if pieceUsed q piece || Just piece == picked
     then char defAttr '·' <|> charFill defAttr ' ' 3 1
     else char keyAttr (intToDigit (pieceId piece)) <|>
          char defAttr ' ' <|>
          pieceGlyph piece
  where
    keyAttr =
      case inp of
        PosnInput{} -> activeAttr
        _           -> inactiveAttr

    picked = case inp of
                PosnPieceInput _ p -> Just p
                _                  -> Nothing

drawHint :: Maybe (Int, Result) -> Maybe (Posn, Piece) -> Image
drawHint search hint =
  horizCat [searchPart, movePart]

  where
    searchPart =
      case search of
        Nothing         -> emptyImage
        Just (depth, W) -> string defAttr ("Win in " ++ show depth)
        Just (depth, D) -> string defAttr ("Draw in " ++ show depth)
        Just (depth, L) -> string defAttr ("Lose in " ++ show depth)

    movePart =
      case hint of
        Nothing -> emptyImage
        Just (posn, piece) ->
          string defAttr ": position " <|>
          drawPosn posn                <|>
          string defAttr " and piece " <|>
          drawPiece piece


drawPosn :: Posn -> Image
drawPosn p =
  char activeAttr (intToDigit (posnId p))

drawPiece :: Piece -> Image
drawPiece p =
  char activeAttr (intToDigit (pieceId p)) <|>
  string defAttr " ["                      <|>
  pieceGlyph p                             <|>
  char defAttr ']'

pieceGlyph :: Piece -> Image
pieceGlyph (Piece p) = string (defAttr `withForeColor` color) str
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
        _ -> error "pieceGlyph: impossible"

-- | Iterative-deepening depth-first search of the game tree.
-- This action will run up to the given amount of time printing
-- outcomes of searches at increasing depths until time expires
-- or a conclusive outcome is determined.
ai ::
  TQueue (Int,Outcome) {- ^ output channel -} ->
  Quarto {- ^ game configuration -} ->
  IO (Async ())
ai tchan q = async (loop 1)
  where
    maxDepth = length (filter (not . positionCovered q . Posn) [0..0xf])

    loop depth =
      do let outcome = bestOutcome depth q
         evaluate outcome
         atomically (writeTQueue tchan (depth, outcome))
         case outcome of
           Draw (_:_) | depth < maxDepth -> loop (depth+1)
           _                             -> return ()
