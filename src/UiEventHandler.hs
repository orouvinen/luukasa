{-
    Intermediate event handlers.
    These will act as a bridge from IO monad to pure code.

    These will:
        1. read in current mutable state from IORef
        2. get all necessary data from the EventWhatever

        In case of event that is purely UI specific (such as ViewScale change etc.)
            3. update state directly without going any further

        In case of domain specific action,
            3. call dispatchAction and
            4. replace mutable state with state returned by dispatchAction
-}
module UiEventHandler where

import           AppState
import           Data.IORef
import           EventHandler as E (Event (..), dispatchAction)
import qualified GI.Gdk       as Gdk
-- import qualified GI.Gdk.Objects as GO

canvasMouseButtonClick :: IORef AppState -> Gdk.EventButton -> IO Bool
canvasMouseButtonClick s e = do
    appState <- readIORef s

    x <- truncate <$> Gdk.getEventButtonX e
    y <- truncate <$> Gdk.getEventButtonY e

    let dispatch = dispatchAction appState

    print $ actionState appState

    let newState = case actionState appState of
           PlacingNewJoint -> dispatch $ E.CreateJoint x y
           Idle            -> dispatch $ E.TrySelect x y
           _               -> appState

    writeIORef s newState { actionState = Idle }
    return True

canvasKeyPress :: IORef AppState -> Gdk.EventKey -> IO Bool
canvasKeyPress s eventKey = do
    appState <- readIORef s

    key <- Gdk.getEventKeyKeyval eventKey >>= Gdk.keyvalToUpper

    print $ actionState appState

    let newState = case key of
            Gdk.KEY_J ->
                if selectionSize appState == 1 -- Parent joint needs to be selected
                    then appState { actionState = PlacingNewJoint }
                    else appState -- TODO: notify about the need of joint selection prior to the command

            _         -> appState

    writeIORef s newState
    return True


scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.01

canvasScrollWheel :: IORef AppState -> Gdk.EventScroll -> IO Bool
canvasScrollWheel s eventScroll = do
    appState <- readIORef s
    scrollDirection <- Gdk.getEventScrollDirection eventScroll

    let scaleChange = if scrollDirection == Gdk.ScrollDirectionDown
        then -scrollWheelScaleStep
        else scrollWheelScaleStep

    let newState = appState { viewScale = viewScale appState + scaleChange }
    writeIORef s newState

    print $ viewScale newState

    return True


setViewScale :: IORef AppState -> Double -> IO Bool
setViewScale s scaleFactor = do
    state <- readIORef s

    let newState = state { viewScale = scaleFactor }

    writeIORef s newState
    return True

setViewTranslate :: IORef AppState -> Int -> Int -> IO Bool
setViewTranslate s trX trY = do
    state <- readIORef s

    let newState = state { viewTranslate = (trX, trY) }

    writeIORef s newState
    return True

