{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Luukasa.EventHandler where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (decode, encode)
import qualified Data.ByteString.Lazy   as BS (readFile, writeFile)
import           Data.GI.Base           (AttrOp ((:=)), new)
import           Data.Maybe             (fromJust)
import qualified GI.Gdk                 as Gdk
import qualified GI.Gtk                 as Gtk
import           Luukasa.Animation
import           Luukasa.AppState       as ST
import           Luukasa.Common
import           Luukasa.EditorAction   as E (Action (..), SelectMode (..),
                                              dispatchAction)
import           Luukasa.Event.Keyboard
import           Luukasa.Event.Mouse    (HasMouseEvent, clickModifiers,
                                         clickPos, getScrollDirection,
                                         motionModifiers, motionPos)
import           Luukasa.Joint          (JointLockMode (..))

updateAppState :: HasAppState m => Either a AppState -> m ()
updateAppState result = do
    case result of
        Left _         -> return ()
        Right newState -> put newState

selectLockMode :: HasAppState m => JointLockMode -> m ()
selectLockMode lockMode = do
    appState <- get
    put appState { jointLockMode = lockMode }


canvasPrimaryMouseButtonClick :: (HasAppState m, HasMouseEvent m) => Gdk.EventButton -> m (Either ErrorMessage AppState)
canvasPrimaryMouseButtonClick e = do
    appState <- get

    (x', y') <- clickPos e
    let x = truncate x'
        y = truncate y'

    ctrlPressed <- elem Gdk.ModifierTypeControlMask <$> clickModifiers e

    let dispatch = dispatchAction appState
        result = case actionState appState of
            PlacingNewJoint -> dispatch $ E.CreateJoint x y
            Idle            -> dispatch $ E.TrySelect x y (if ctrlPressed then Toggle else Set)
            _               -> Right appState

    updateAppState result
    return result


canvasPrimaryMouseButtonRelease :: HasAppState m => Gdk.EventButton -> m ()
canvasPrimaryMouseButtonRelease _ = do
    appState <- get

    if isPlaybackOn appState
        then return ()
        else put appState { actionState = Idle }

startPlayback :: HasAppState m => TimerCallbackId -> TimestampUs -> m ()
startPlayback timerCallbackId timestamp = do
    appState <- get
    put appState { actionState = AnimationPlayback timerCallbackId
                 , frameStart = Just timestamp
                 }

stopPlayback :: HasAppState m => m ()
stopPlayback = do
    appState <- get
    put appState { actionState = Idle
                 , frameStart = Nothing
                 }

playbackTick :: HasAppState m => TimestampUs -> m Bool
playbackTick timestamp = do
    s <- get
    let frameIntervalUs = (1.0 / (fromIntegral (fps $ animation s) :: Double)) * 1000 * 1000
        lastFrame = fromJust (ST.frameStart s)
        sinceLastFrame = timestamp - lastFrame
        renderNeeded = fromIntegral sinceLastFrame >= frameIntervalUs

    when renderNeeded $ do
        -- timestamp might not be totally accurate for frameStart if
        -- this frame is delayed. Should probably set the originally intended frameStart?
        let result = dispatchAction s { frameStart = Just timestamp } (E.FrameStep 1)
        updateAppState result

    return renderNeeded

canvasKeyPress :: (HasAppState m, HasKeyEvent m) => Gdk.EventKey -> m ()
canvasKeyPress eventKey = do
    appState <- get
    key <- getKey eventKey

    -- print $ actionState appState

    let dispatch = dispatchAction appState
    --     debugJoints = printJoints appState
    --     debugState = printState appState

    -- liftIO $ putStr $ case key of
    --     Gdk.KEY_1 -> "JOINTS: " ++ debugJoints
    --     Gdk.KEY_2 -> "STATE: " ++ debugState
    --     _         -> ""

    let result = case key of
            Gdk.KEY_J       ->
                if selectionSize appState == 1 -- Parent joint needs to be selected
                    then Right appState { actionState = PlacingNewJoint }
                    else Left "No joint selected" -- TODO: this is not the kind of error we're looking for

            Gdk.KEY_Up          -> dispatch $ E.RotateSelected 2
            Gdk.KEY_Down        -> dispatch $ E.RotateSelected (-2)
            Gdk.KEY_KP_Add      -> dispatch E.CreateFrame
            Gdk.KEY_KP_Subtract -> dispatch E.DeleteFrame
            Gdk.KEY_Left        -> dispatch $ E.FrameStep (-1)
            Gdk.KEY_Right       -> dispatch $ E.FrameStep 1
            _                   -> Right appState

    updateAppState result

scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: (HasAppState m, HasMouseEvent m) => Gdk.EventScroll -> m ()
canvasScrollWheel eventScroll = do
    appState <- get
    scrollDirection <- getScrollDirection eventScroll

    let scaleChange =
            if scrollDirection == Gdk.ScrollDirectionDown
            then -scrollWheelScaleStep
            else scrollWheelScaleStep

    let newState = appState { viewScale = viewScale appState + scaleChange }
    put newState

canvasMouseMotion :: (HasAppState m, HasMouseEvent m) => Gdk.EventMotion -> m (Either ErrorMessage AppState)
canvasMouseMotion e = do
    appState <- get

    mouseBtnPressed <- elem Gdk.ModifierTypeButton1Mask <$> motionModifiers e

    if not mouseBtnPressed || selectionSize appState /= 1
        then return $ Right appState
        else do
            (mouseX, mouseY) <- motionPos e

            let dragState =
                    if selectionSize appState == 0
                        then DragSelectionRect
                        else DragSelected (dragMode appState)

                action = case dragState of
                    DragSelectionRect -> E.ExtendSelectionRect mouseX mouseY
                    DragSelected DragMove -> E.MoveSelected mouseX mouseY
                    -- Just a placeholder for now
                    DragSelected DragRotate -> E.DragRotateSelected mouseX mouseY

            let result = E.dispatchAction appState { actionState = ST.Drag dragState } action
            updateAppState result
            return result

setViewScale :: HasAppState m => Double -> m ()
setViewScale scaleFactor = do
    state <- get
    put state { viewScale = scaleFactor }

setViewTranslate :: HasAppState m => Double -> Double -> m ()
setViewTranslate trX trY = do
    state <- get
    put state { translateX = trX, translateY = trY }

menuSave :: (HasAppState m, MonadIO m) => Gtk.Window -> m ()
menuSave w = do
    state <- get
    filename <- saveFileChooserDialog w

    case filename of
        Nothing -> return ()
        Just f  -> do
            let json = encode $ ST.animation state
            liftIO $ BS.writeFile f json


menuOpen :: (HasAppState m, MonadIO m) => Gtk.Window -> m ()
menuOpen w = do
    state <- get
    filename <- openFileChooserDialog w

    case filename of
        Nothing -> return ()
        Just f -> do
            json <- liftIO $ BS.readFile f
            let animation' = decode json

            case animation' of
                Nothing -> return ()
                Just a  -> put state { ST.animation = a }


saveFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe String)
saveFileChooserDialog = fileChooserDialog Gtk.FileChooserActionSave

openFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe String)
openFileChooserDialog = fileChooserDialog Gtk.FileChooserActionOpen

fileChooserDialog :: MonadIO m => Gtk.FileChooserAction -> Gtk.Window -> m (Maybe String)
fileChooserDialog actionType mainWindow = do
    dlg <- new Gtk.FileChooserDialog [ #title := "Save animation"
                                     , #action := actionType
                                     ]

    Gtk.windowSetTransientFor dlg $ Just mainWindow
    _ <- Gtk.dialogAddButton dlg "gtk-save" $ (toEnum . fromEnum) Gtk.ResponseTypeAccept
    _ <- Gtk.dialogAddButton dlg "gtk-cancel" $ (toEnum . fromEnum) Gtk.ResponseTypeCancel

    Gtk.widgetShow dlg
    _ <- Gtk.dialogRun dlg

    filename <- Gtk.fileChooserGetFilename dlg
    Gtk.widgetDestroy dlg
    return filename

