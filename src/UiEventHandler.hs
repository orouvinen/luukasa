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
import           EventHandler  as E (Event (..), SelectMode (..),
                                     dispatchAction)
import qualified GI.Gdk        as Gdk
-- import qualified GI.Gdk.Objects as GO

import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (toList)
import           Data.Maybe    (fromJust)
import qualified Joint         as J
import qualified Tree          as T

canvasMouseButtonClick :: IORef ST.AppState -> Gdk.EventButton -> IO Bool
canvasMouseButtonClick s e = do
    appState <- readIORef s

    x <- truncate <$> Gdk.getEventButtonX e
    y <- truncate <$> Gdk.getEventButtonY e

    let dispatch = dispatchAction appState

    eventState <- e `Gdk.get` #state
    let ctrlPressed = Gdk.ModifierTypeControlMask `elem` eventState
    let selectMode = if ctrlPressed then Toggle else Set

    let newState = case ST.actionState appState of
           ST.PlacingNewJoint -> dispatch $ E.CreateJoint x y
           ST.Idle            -> dispatch $ E.TrySelect x y selectMode
           _                  -> appState

    writeIORef s newState { ST.actionState = ST.Idle }
    return False

canvasKeyPress :: IORef ST.AppState -> Gdk.EventKey -> IO Bool
canvasKeyPress s eventKey = do
    appState <- readIORef s

    key <- Gdk.getEventKeyKeyval eventKey >>= Gdk.keyvalToUpper

    -- print $ actionState appState

    let dispatch = dispatchAction appState
    let debugJoints = ST.printJoints appState
    let debugState = ST.printState appState

    putStr $ case key of
        Gdk.KEY_1 -> "JOINTS: " ++ debugJoints
        Gdk.KEY_2 -> "STATE: " ++ debugState
        _         -> ""

    let newState = case key of
            Gdk.KEY_J       ->
                if ST.selectionSize appState == 1 -- Parent joint needs to be selected
                    then appState { ST.actionState = ST.PlacingNewJoint }
                    else appState -- TODO: notify about the need of joint selection prior to the command

            Gdk.KEY_Up      -> dispatch $ E.RotateSelected 10
            Gdk.KEY_Down    -> dispatch $ E.RotateSelected (-10)
            _               -> appState

    writeIORef s newState
    return False


scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: IORef ST.AppState -> Gdk.EventScroll -> IO Bool
canvasScrollWheel s eventScroll = do
    appState <- readIORef s
    scrollDirection <- Gdk.getEventScrollDirection eventScroll

    let scaleChange = if scrollDirection == Gdk.ScrollDirectionDown
        then -scrollWheelScaleStep
        else scrollWheelScaleStep

    let newState = appState { ST.viewScale = ST.viewScale appState + scaleChange }
    writeIORef s newState

    return False


setViewScale :: IORef ST.AppState -> Double -> IO Bool
setViewScale s scaleFactor = do
    state <- readIORef s

    let newState = state { ST.viewScale = scaleFactor }

    writeIORef s newState
    return False

setViewTranslate :: IORef ST.AppState -> Int -> Int -> IO Bool
setViewTranslate s trX trY = do
    state <- readIORef s

    let newState = state { ST.viewTranslate = (trX, trY) }

    writeIORef s newState
    return False

