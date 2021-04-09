{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Luukasa.UiEventHandler where

import           Data.IORef
import qualified GI.Gdk               as Gdk
-- import qualified GI.Gdk.Objects as GO
import qualified Luukasa.Animation    as A
import           Luukasa.AppState     as ST
import           Luukasa.EventHandler as E (Event (..), SelectMode (..),
                                            dispatchAction)

updateAppState :: Show a => IORef AppState -> Either a AppState -> IO ()
updateAppState stateRef result =
    case result of
        Left err       -> print err
        Right newState -> writeIORef stateRef newState

canvasMouseButtonClick :: IORef AppState -> Gdk.EventButton -> IO ()
canvasMouseButtonClick s e = do
    appState <- readIORef s

    x <- truncate <$> Gdk.getEventButtonX e
    y <- truncate <$> Gdk.getEventButtonY e

    ctrlPressed <- e `Gdk.get` #state >>= (return . elem Gdk.ModifierTypeControlMask)

    let dispatch = dispatchAction appState
        applyResult = updateAppState s
        result = case actionState appState of
            PlacingNewJoint -> dispatch $ E.CreateJoint x y
            Idle            -> dispatch $ E.TrySelect x y (if ctrlPressed then Toggle else Set)
            _               -> Right appState

    applyResult result

canvasMouseButtonRelease :: IORef AppState -> Gdk.EventButton -> IO ()
canvasMouseButtonRelease s e = do
    appState <- readIORef s

    let newState = appState { actionState = Idle }

    writeIORef s newState


canvasKeyPress :: IORef AppState -> Gdk.EventKey -> IO ()
canvasKeyPress s eventKey = do
    appState <- readIORef s

    key <- Gdk.getEventKeyKeyval eventKey >>= Gdk.keyvalToUpper

    -- print $ actionState appState

    let dispatch = dispatchAction appState
        applyResult = updateAppState s
        debugJoints = printJoints appState
        debugState = printState appState

    putStr $ case key of
        Gdk.KEY_1 -> "JOINTS: " ++ debugJoints
        Gdk.KEY_2 -> "STATE: " ++ debugState
        _         -> ""

    let result = case key of
            Gdk.KEY_J       ->
                if selectionSize appState == 1 -- Parent joint needs to be selected
                    then Right appState { actionState = PlacingNewJoint }
                    else Left "No joint selected" -- TODO: this is not the kind of error we're looking for

            Gdk.KEY_Up          -> dispatch $ E.RotateSelected 10
            Gdk.KEY_Down        -> dispatch $ E.RotateSelected (-10)
            Gdk.KEY_KP_Add      -> dispatch E.CreateFrame
            Gdk.KEY_KP_Subtract -> dispatch E.DeleteFrame
            Gdk.KEY_Left        -> dispatch $ E.FrameStep (-1)
            Gdk.KEY_Right       -> dispatch $ E.FrameStep 1
            _                   -> Right appState

    applyResult result

scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: IORef AppState -> Gdk.EventScroll -> IO ()
canvasScrollWheel s eventScroll = do
    appState <- readIORef s
    scrollDirection <- Gdk.getEventScrollDirection eventScroll

    let scaleChange =
            if scrollDirection == Gdk.ScrollDirectionDown
            then -scrollWheelScaleStep
            else scrollWheelScaleStep

    let newState = appState { viewScale = viewScale appState + scaleChange }
    writeIORef s newState


canvasMouseMotion :: IORef AppState -> Gdk.EventMotion -> IO ()
canvasMouseMotion s e = do
    appState <- readIORef s

    mouseBtnPressed <- e `Gdk.get` #state >>= (return . elem Gdk.ModifierTypeButton1Mask)

    let applyResult = updateAppState s

    if not mouseBtnPressed || selectionSize appState /= 1
        then return ()
        else do
            mouseX <- e `Gdk.get` #x
            mouseY <- e `Gdk.get` #y

            let dragState =
                    if selectionSize appState == 0
                        then DragSelectionRect
                        else DragSelected (dragMode appState)

                action = case dragState of
                    DragSelectionRect -> E.ExtendSelectionRect mouseX mouseY
                    DragSelected DragMove -> E.MoveSelected mouseX mouseY
                    -- Just a placeholder for now
                    DragSelected DragRotate -> E.DragRotateSelected mouseX mouseY

            let result = E.dispatchAction appState { actionState = dragState } action
            applyResult result

setViewScale :: IORef AppState -> Double -> IO ()
setViewScale s scaleFactor = do
    state <- readIORef s

    let newState = state { viewScale = scaleFactor }

    writeIORef s newState

setViewTranslate :: IORef AppState -> Double -> Double -> IO ()
setViewTranslate s trX trY = do
    state <- readIORef s

    let newState = state { translateX = trX, translateY = trY }

    writeIORef s newState
