{-# LANGUAGE OverloadedLabels #-}
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

import           Data.IORef
import           EventHandler as E (Event (..), SelectMode (..), dispatchAction)
import qualified GI.Gdk       as Gdk
-- import qualified GI.Gdk.Objects as GO

import qualified Animation    as A
import           AppState

canvasMouseButtonClick :: IORef AppState -> Gdk.EventButton -> IO Bool
canvasMouseButtonClick s e = do
    appState <- readIORef s

    x <- truncate <$> Gdk.getEventButtonX e
    y <- truncate <$> Gdk.getEventButtonY e

    let dispatch = dispatchAction appState

    ctrlPressed <- e `Gdk.get` #state >>= (return . elem Gdk.ModifierTypeControlMask)

    let newState = case actionState appState of
           PlacingNewJoint -> dispatch $ E.CreateJoint x y
           Idle            -> dispatch $ E.TrySelect x y (if ctrlPressed then Toggle else Set)
           _                  -> appState

    writeIORef s newState { actionState = Idle }
    return False

canvasMouseButtonRelease :: IORef AppState -> Gdk.EventButton -> IO Bool
canvasMouseButtonRelease s e = do
    appState <- readIORef s

    let newState = appState { actionState = Idle }

    writeIORef s newState
    return False


canvasKeyPress :: IORef AppState -> Gdk.EventKey -> IO Bool
canvasKeyPress s eventKey = do
    appState <- readIORef s

    key <- Gdk.getEventKeyKeyval eventKey >>= Gdk.keyvalToUpper

    -- print $ actionState appState

    let dispatch = dispatchAction appState
    let debugJoints = printJoints appState
    let debugState = printState appState

    putStr $ case key of
        Gdk.KEY_1 -> "JOINTS: " ++ debugJoints
        Gdk.KEY_2 -> "STATE: " ++ debugState
        _         -> ""

    let newState = case key of
            Gdk.KEY_J       ->
                if selectionSize appState == 1 -- Parent joint needs to be selected
                    then appState { actionState = PlacingNewJoint }
                    else appState -- TODO: notify about the need of joint selection prior to the command

            Gdk.KEY_Up          -> dispatch $ E.RotateSelected 10
            Gdk.KEY_Down        -> dispatch $ E.RotateSelected (-10)
            Gdk.KEY_KP_Add      -> dispatch E.CreateFrame
            Gdk.KEY_KP_Subtract -> dispatch E.DeleteFrame
            Gdk.KEY_Left        -> dispatch $ E.ShowFrame $ A.currentFrame (animation appState) - 1
            Gdk.KEY_Right       -> dispatch $ E.ShowFrame $ A.currentFrame (animation appState) + 1
            _                   -> appState

    writeIORef s newState
    return False


scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: IORef AppState -> Gdk.EventScroll -> IO Bool
canvasScrollWheel s eventScroll = do
    appState <- readIORef s
    scrollDirection <- Gdk.getEventScrollDirection eventScroll

    let scaleChange = if scrollDirection == Gdk.ScrollDirectionDown
        then -scrollWheelScaleStep
        else scrollWheelScaleStep

    let newState = appState { viewScale = viewScale appState + scaleChange }
    writeIORef s newState

    return False

canvasMouseMotion :: IORef AppState -> Gdk.EventMotion -> IO Bool
canvasMouseMotion s e = do
    appState <- readIORef s

    mouseBtnPressed <- e `Gdk.get` #state >>= (return . elem Gdk.ModifierTypeButton1Mask)

    -- for now, only allow dragging single selected joint
    newState <- if not mouseBtnPressed || selectionSize appState /= 1
        then return appState
        else do
            mouseX <- e `Gdk.get` #x
            mouseY <- e `Gdk.get` #y

            let dragState = if selectionSize appState == 0
                then DragSelectionRect
                else DragSelected (dragMode appState)

            let action = case dragState of
                    DragSelectionRect -> E.ExtendSelectionRect mouseX mouseY
                    DragSelected DragMove -> E.MoveSelected mouseX mouseY
                    -- Just a placeholder for now
                    DragSelected DragRotate -> E.DragRotateSelected mouseX mouseY

            return $
                (E.dispatchAction appState action) { actionState = dragState }

    -- print $ actionState appState
    writeIORef s newState
    return False


setViewScale :: IORef AppState -> Double -> IO Bool
setViewScale s scaleFactor = do
    state <- readIORef s

    let newState = state { viewScale = scaleFactor }

    writeIORef s newState
    return False

setViewTranslate :: IORef AppState -> Double -> Double -> IO Bool
setViewTranslate s trX trY = do
    state <- readIORef s

    let newState = state { translateX = trX, translateY = trY }

    writeIORef s newState
    return False

