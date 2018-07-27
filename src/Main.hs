module Main (main) where

-- base
import Control.Concurrent.Async (Async, async, cancel, waitCatchSTM)
import Control.Exception        (bracket, evaluate)
import Control.Monad            (guard, unless)
import Data.Char                (intToDigit)
import Data.Foldable            (asum, traverse_)
import Data.List                (intersperse)
import Data.Maybe               (fromMaybe, listToMaybe, isJust)

-- random
import System.Random (randomRIO)

-- stm
import Control.Concurrent.STM
         (STM, TQueue, newTQueueIO, readTChan, readTQueue, atomically,
          retry, writeTQueue, registerDelay, readTVar)

-- vty
import Graphics.Vty (Config(..), Vty(..), mkVty, picForImage)
import Graphics.Vty.Attributes
import Graphics.Vty.Input

-- local
import Coord
import BoxDrawing
import Game
import Drawing

-- | State of application
data App = App
  { appVty     :: Vty                   -- ^ terminal handle
  , appQuarto  :: Quarto                -- ^ game board
  , appMode    :: Modality              -- ^ application input mode
  , appSearch  :: Maybe (Int, Result)   -- ^ most recent search result
  , appHint    :: Maybe (Posn, Piece)   -- ^ most recent move hint
  , appAsync   :: Maybe (Async ())      -- ^ handle to search thread
  , appChannel :: TQueue (Int, Outcome) -- ^ results from search thread
  , appTimer   :: STM ()                -- ^ stm action that succeeds when timer ready
  , appCounter :: !Int                  -- ^ counter used to animate game over
  }

-- | The result of a search can indicate: Win, Lose, and Draw
data Result = W | L | D

-- | Type of modes of the application. These determine how the application
-- will respond to terminal inputs.
data Modality
  = Active (Maybe Posn) (Maybe Piece)
  | GameOver [Posn] -- list of positions to highlight
  deriving Show

-- | Predicate for the 'GameOver' mode.
isGameOver :: Modality -> Bool
isGameOver GameOver{} = True
isGameOver _          = False

-- common UI attributes ------------------------------------------------

-- | Attribute indicating that a key can be pressed
activeAttr :: Attr
activeAttr = defAttr `withStyle` bold

-- | Attribute indicating that a key can be pressed in other situations
inactiveAttr :: Attr
inactiveAttr = defAttr `withForeColor` Color240 224

-- character representations of pieces and positions -------------------

pieceToChar :: Piece -> Char
pieceToChar = intToDigit . pieceId

posnToChar :: Posn -> Char
posnToChar = intToDigit . posnId

------------------------------------------------------------------------

vtyConfig :: Config
vtyConfig = mempty { mouseMode = Just True }

-- | Play a game of Quarto on the terminal.
main :: IO ()
main =
  newTQueueIO >>= \tchan ->
  bracket (mkVty vtyConfig) shutdown $ \vty ->
  main' App
    { appVty     = vty
    , appQuarto  = initialQuarto
    , appMode    = Active Nothing Nothing
    , appSearch  = Nothing
    , appHint    = Nothing
    , appAsync   = Nothing
    , appChannel = tchan
    , appTimer   = noTimer
    , appCounter = 0 }


-- application events --------------------------------------------------

-- | Type of all events that the application can respond to.
data AppEvent
  = VtyEvent Event          -- terminal event
  | HintEvent (Int,Outcome) -- result from search
  | AsyncFinished           -- search thread exited
  | TimerEvent              -- timer fired

data UIEvent
  = PickPiece Piece
  | PickPosn Posn
  | Clear
  | Confirm
  | NewGame
  | UseHint
  | StartSearch
  | StopSearch
  | Quit

-- | Get the next available application event.
getEvent :: App -> IO AppEvent
getEvent app =
  atomically $ asum
    [ VtyEvent      <$> readTChan (_eventChannel (inputIface (appVty app)))
    , HintEvent     <$> readTQueue (appChannel app)
    , AsyncFinished <$  maybe retry waitCatchSTM (appAsync app)
    , TimerEvent    <$  appTimer app ]

------------------------------------------------------------------------

-- | Application event loop
main' :: App -> IO ()
main' app =
  do doDraw app
     ev <- getEvent app
     case ev of
       AsyncFinished              -> main' app { appAsync = Nothing }
       HintEvent (depth, outcome) -> main' =<< doHintEvent app depth outcome
       VtyEvent event             -> resume =<< doVtyEvent app event
       TimerEvent                 -> main' =<< doTimerEvent app
  where
    resume Nothing     = return ()
    resume (Just app') = main' app'


doVtyEvent :: App -> Event -> IO (Maybe App)
doVtyEvent app event =
  case event of
    EvKey KEsc [] -> return Nothing
    EvMouseDown x y BLeft []
      | Just uiEvent <- getClickEvent app x y -> doUIEvent app uiEvent
    _ -> return (Just app)

getClickEvent :: App -> Int -> Int -> Maybe UIEvent
getClickEvent app x y
  | null keys = Nothing
  | otherwise = Just $! last keys
  where
    keys = coordRegions (C x y) (drawApp app)


doTimerEvent :: App -> IO App
doTimerEvent app =
  do timer <- newTimer
     return app
       { appTimer = timer
       , appCounter = appCounter app + 1 }

-- | Handle the events generated by the hint search thread.
doHintEvent ::
  App     {- ^ application state             -} ->
  Int     {- ^ hint search depth             -} ->
  Outcome {- ^ hint search outcome           -} ->
  IO App  {- ^ application updated with hint -}
doHintEvent app depth outcome =
  case outcome of

    Draw [] ->
      return app
        { appHint   = Nothing
        , appSearch = Just (depth, D) }

    Draw xs ->
      do i <- randomRIO (0, length xs - 1)
         return app
           { appHint   = Just (xs !! i)
           , appSearch = Just (depth, D) }

    Win posn piece ->
      return app
        { appHint   = Just (posn, piece)
        , appSearch = Just (depth, W) }

    Lose ->
      return app { appSearch = Just (depth, L) }


-- | Handle the events generated by the terminal
doUIEvent :: App -> UIEvent -> IO (Maybe App)
doUIEvent app event =
  let continue = return . Just in
  case event of

    -- Quit
    Quit -> return Nothing -- exit event loop

    -- Clear previous choices
    Clear | Active{} <- appMode app -> continue app { appMode = Active Nothing Nothing }

    -- Start AI
    StartSearch -> continue =<< doAI app

    -- Stop AI
    StopSearch ->
      do traverse_ cancel (appAsync app)
         continue app { appAsync = Nothing }

    -- Use hint as input
    UseHint
      | Just (posn, piece) <- appHint app ->
        continue app { appMode = Active (Just posn) (Just piece) }

    -- Restart
    NewGame ->
      do traverse_ cancel (appAsync app)
         continue app
           { appAsync  = Nothing
           , appHint   = Nothing
           , appSearch = Nothing
           , appQuarto = initialQuarto
           , appMode   = Active Nothing Nothing
           , appTimer  = noTimer }

    -- 1. Choose a position
    PickPosn posn
      | Active _ piece <- appMode app
      , not (positionCovered (appQuarto app) posn) ->
      continue app { appMode = Active (Just posn) piece }

    -- 2. Choose a piece
    PickPiece piece
      | Active posn _ <- appMode app
      , not (pieceUsed (appQuarto app) piece) ->
      continue app { appMode = Active posn (Just piece) }

    -- 3. Confirm placement
    Confirm
      | Active (Just posn) (Just piece) <- appMode app ->
      do let quarto   = setPieceAt piece posn (appQuarto app)
             gameWon = checkWinAt posn quarto
             newMode
               | gameWon =
                   GameOver (fromMaybe [] (winningPositions posn quarto))
               | isBoardFull quarto = GameOver []
               | otherwise = Active Nothing Nothing
         timer <- if gameWon then newTimer else return noTimer
         traverse_ cancel (appAsync app)
         continue app
           { appMode   = newMode
           , appQuarto = quarto
           , appHint   = Nothing
           , appSearch = Nothing
           , appAsync  = Nothing
           , appCounter = 0
           , appTimer  = timer }

    -- Ignore everything else
    _ -> continue app

-- Timers --------------------------------------------------------------

newTimer :: IO (STM ())
newTimer =
  do var <- registerDelay 400000
     return (flip unless retry =<< readTVar var)

noTimer :: STM ()
noTimer = retry


-- Application UI rendering --------------------------------------------

-- | Update the terminal display with the current application rendering.
doDraw :: App -> IO ()
doDraw app = update (appVty app) (picForImage (drawingToImage (drawApp app)))

-- | Render the whole UI.
drawApp :: App -> Drawing UIEvent
drawApp app =
  string defAttr " Game Board  -  Available Pieces" :---

  drawQuarto (appCounter app) (appMode app) (appQuarto app) :|||
  pieceKey   (appMode app) (appQuarto app) :---

  drawCommands app :---

  if isGameOver (appMode app)
    then emptyImage
    else drawHint (appSearch app) (appHint app)

-- | Render the currently available keyboard commands.
drawCommands :: App -> Drawing UIEvent
drawCommands app = modePart :--- build commonButtons
  where
    build = horizCat . intersperse (char defAttr ' ')

    commonButtons = [quitButton, newgameButton, hintButton, searchButton]

    button action txt = maybe id Region action img
      where
        attr = if isJust action then activeAttr else inactiveAttr
        img = char defAttr '[' :|||
              string attr txt  :|||
              char defAttr ']'

    quitButton    = button (Just Quit) "quit"
    newgameButton = button (Just NewGame) "new-game"
    hintButton    = button (UseHint <$ appHint app) "use-hint"
    searchButton =
      case appAsync app of
        Just{} -> button (Just StopSearch) "stop-search"
        Nothing -> button (StartSearch <$ guard (not (isGameOver (appMode app)))) "start-search"

    confirmButton =
      case appMode app of
        Active Just{} Just{} -> button (Just Confirm) "confirm"
        _                    -> button Nothing "confirm"

    clearButton =
      case appMode app of
        Active Just{} _ -> button (Just Clear) "clear"
        Active _ Just{} -> button (Just Clear) "clear"
        _               -> button Nothing "clear"

    modePart =
      case appMode app of
        GameOver{}           -> string defAttr "Game Over"
        Active{}             -> build [confirmButton, clearButton]


-- | Render the game board.
drawQuarto :: Int -> Modality -> Quarto -> Drawing UIEvent
drawQuarto counter inp q = renderGrid 4 4 2 edge cell
  where
    selected
      = fmap (\x -> C (x`mod`4) (x`div`4))
      $ fmap posnId
      $ case inp of
          Active p _  -> p
          GameOver ps -> listToMaybe (drop (counter `mod` 4) ps)

    -- boundary of selection
    edge c Horiz = edge1 c up
    edge c Vert  = edge1 c left
    edge1 c f
      | (c `elem` selected) /= (f c `elem` selected) = Just Heavy
      | otherwise                                    = Just Thin

    cell (C col row) =
      let posn = Posn (row * 4 + col) in
      case inp of
        Active (Just p) Nothing | p == posn ->
          string (defAttr `withForeColor` magenta) "??"
        Active (Just p) (Just piece) | p == posn ->
          pieceGlyph piece
        _ ->
          case pieceAt q posn of
            Nothing    -> Region (PickPosn posn) (string defAttr "  ")
            Just piece -> pieceGlyph piece

-- | Draw the available pieces and corresponding piece IDs
pieceKey :: Modality -> Quarto -> Drawing UIEvent
pieceKey inp q = renderGrid 4 4 2 edge  $ \(C col row) ->
  let piece = Piece (row*4+col) in
  if pieceUsed q piece || Just piece == picked
     then char defAttr '·' :||| char defAttr ' '
     else Region (PickPiece piece) (pieceGlyph piece)
  where
    selected =
      case inp of
        Active Nothing (Just (Piece p)) -> Just (C (p`mod`4) (p`div`4))
        _ -> Nothing

    edge c Horiz = edge1 c up
    edge c Vert  = edge1 c left
    edge1 c f
      | (c `elem` selected) /= (f c `elem` selected) = Just Thin
      | otherwise                                    = Nothing

    picked = case inp of
                Active Just{} p -> p
                _               -> Nothing

-- | Render the hint search results.
drawHint :: Maybe (Int, Result) -> Maybe (Posn, Piece) -> Drawing UIEvent
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
          string defAttr ": position "          :|||
          char   activeAttr (posnToChar posn)   :|||
          string defAttr " and piece "          :|||
          char   activeAttr (pieceToChar piece) :|||
          string defAttr " ["                   :|||
          pieceGlyph piece                      :|||
          char   defAttr ']'

-- | Render a single piece as a glyph capturing its various attributes.
pieceGlyph :: Piece -> Drawing UIEvent
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

-- incremental hint search ---------------------------------------------

-- | Cancel any previous search thread and create a new one for the current
-- game state.
doAI :: App -> IO App
doAI app =
  do traverse_ cancel (appAsync app)
     if isGameOver (appMode app)
       then return app
       else do thread <- async (ai (appChannel app) (appQuarto app))
               return app { appAsync = Just thread }

-- | Iterative-deepening depth-first search of the game tree.
-- This action will run up to the given amount of time printing
-- outcomes of searches at increasing depths until time expires
-- or a conclusive outcome is determined.
ai ::
  TQueue (Int,Outcome) {- ^ output channel     -} ->
  Quarto               {- ^ game configuration -} ->
  IO ()
ai tchan q = loop 1
  where
    maxDepth = length (filter (not . positionCovered q . Posn) [0..0xf])

    loop depth =
      do let outcome = bestOutcome depth q
         evaluate outcome -- ensure we aren't writing thunks to the queue
         atomically (writeTQueue tchan (depth, outcome))
         case outcome of
           Draw (_:_) | depth < maxDepth -> loop (depth+1)
           _                             -> return ()
