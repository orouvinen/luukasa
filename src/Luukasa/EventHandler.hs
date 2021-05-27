{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Luukasa.EventHandler where

import           Control.Monad          (forM, join, unless, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State    (MonadState, get, gets, modify, put)
import           Data.Aeson             (decode, encode)
import qualified Data.ByteString.Lazy   as BS (readFile, writeFile)
import           Data.Foldable          (forM_)
import           Data.Function          ((&))
import           Data.GI.Base           (AttrOp ((:=)), new)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, pack, unpack)
import qualified GI.Gdk                 as Gdk
import qualified GI.Gtk                 as Gtk
import qualified Luukasa.Animation      as Animation
import           Luukasa.AppState       (ActionState (..), AppState,
                                         DragMode (..), DragState (..))
import qualified Luukasa.AppState       as ST
import           Luukasa.Common         (ErrorMessage, TimerCallbackId,
                                         TimestampUs)
import           Luukasa.EditorAction   as E (Action (..), ActionResult,
                                              SelectMode (..), dispatchAction)
import           Luukasa.Event.Keyboard (HasKeyEvent (..))
import           Luukasa.Event.Mouse    (HasMouseEvent, clickModifiers,
                                         clickPos, getScrollDirection,
                                         motionModifiers, motionPos)
import           Luukasa.Joint          (JointLockMode (..))


type EventResult a = Either ErrorMessage a

jointDragLockModifier, selectToggleModifier :: Gdk.ModifierType
jointDragLockModifier = Gdk.ModifierTypeShiftMask
selectToggleModifier = Gdk.ModifierTypeControlMask


toEventResult :: Monad m => ActionResult -> a -> m (EventResult a)
toEventResult res retval =
    return $ case res of
        Right _  -> Right retval
        Left err -> Left err

updateAppState :: MonadState AppState m => Either a AppState -> m ()
updateAppState = \case
    Left _         -> return ()
    Right newState -> put newState

selectLockMode :: MonadState AppState m => JointLockMode -> m ()
selectLockMode lockMode = modify (\s -> s { ST.jointLockMode = lockMode })

canvasPrimaryMouseButtonClick :: (MonadState AppState m, HasMouseEvent m) => Gdk.EventButton -> m ()
canvasPrimaryMouseButtonClick e = do
    appState <- get

    (x', y') <- clickPos e
    let x = truncate x'
        y = truncate y'

    toggleSelect <- elem selectToggleModifier <$> clickModifiers e

    let dispatch = dispatchAction appState
        result = case ST.actionState appState of
            ST.PlacingNewJoint -> dispatch $ E.CreateJoint x y
            ST.Idle            -> dispatch $ E.TrySelect x y (if toggleSelect then Toggle else Set)
            _               -> Right appState

    updateAppState result


canvasPrimaryMouseButtonRelease :: MonadState AppState m => Gdk.EventButton -> m ()
canvasPrimaryMouseButtonRelease _ = do
    s <- get
    unless (ST.isPlaybackOn s) $ put s { ST.actionState = Idle }

startPlayback :: MonadState AppState m => TimerCallbackId -> TimestampUs -> m ()
startPlayback timerCallbackId timestamp =
    modify (\s -> s { ST.actionState = AnimationPlayback timerCallbackId
                    , ST.frameStart = Just timestamp
                    })

stopPlayback :: MonadState AppState m => m ()
stopPlayback = modify (\s -> s { ST.actionState = Idle, ST.frameStart = Nothing })

playbackTick :: MonadState AppState m => TimestampUs -> m Bool
playbackTick timestamp = do
    s <- get
    let frameIntervalUs = (1.0 / (fromIntegral (Animation.fps $ ST.animation s) :: Double)) * 1000 * 1000
        lastFrame = fromJust (ST.frameStart s)
        sinceLastFrame = timestamp - lastFrame
        renderNeeded = fromIntegral sinceLastFrame >= frameIntervalUs

    when renderNeeded $ do
        -- timestamp might not be totally accurate for frameStart if
        -- this frame is delayed. Should probably set the originally intended frameStart?
        let result = dispatchAction s { ST.frameStart = Just timestamp } (E.FrameStep 1)
        updateAppState result

    return renderNeeded

canvasKeyPress :: (MonadState AppState m, HasKeyEvent m) => Gdk.EventKey -> m (EventResult ())
canvasKeyPress eventKey = do
    s <- get
    key <- getKey eventKey

    let dispatch = dispatchAction s

    let result = case key of
            Gdk.KEY_J       ->
                if ST.selectionSize s == 1 -- Parent joint needs to be selected
                    then Right s { ST.actionState = PlacingNewJoint }
                    else Left "Parent joint needs to be selected in order to create a joint"
            Gdk.KEY_Delete      -> dispatch E.DeleteSelected
            Gdk.KEY_Up          -> dispatch $ E.RotateSelected 2
            Gdk.KEY_Down        -> dispatch $ E.RotateSelected (-2)
            Gdk.KEY_KP_Add      -> dispatch E.CreateFrame
            Gdk.KEY_KP_Subtract -> dispatch E.DeleteFrame
            Gdk.KEY_Left        -> dispatch $ E.FrameStep (-1)
            Gdk.KEY_Right       -> dispatch $ E.FrameStep 1
            _                   -> Right s

    updateAppState result
    toEventResult result ()

scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: (MonadState AppState m, HasMouseEvent m) => Gdk.EventScroll -> m ()
canvasScrollWheel eventScroll = do
    scrollDirection <- getScrollDirection eventScroll

    let scaleChange =
            if scrollDirection == Gdk.ScrollDirectionDown
            then -scrollWheelScaleStep
            else scrollWheelScaleStep

    modify (\s -> s { ST.viewScale = ST.viewScale s + scaleChange })

canvasMouseMotion :: (MonadState AppState m, HasMouseEvent m) => Gdk.EventMotion -> m ()
canvasMouseMotion e = do
    appState <- get

    mouseBtnPressed <- elem Gdk.ModifierTypeButton1Mask <$> motionModifiers e
    toggleDragMode <- elem jointDragLockModifier <$> motionModifiers e

    when (mouseBtnPressed && ST.selectionSize appState == 1) $ do
        (mouseX, mouseY) <- motionPos e
        let dragState =
                if ST.selectionSize appState == 0
                    then DragSelectionRect
                    else DragSelected $ if toggleDragMode then toggledDragMode else defaultDragMode
                        where
                        defaultDragMode = ST.dragMode appState
                        toggledDragMode = case defaultDragMode of
                                                DragMove   -> DragRotate
                                                DragRotate -> DragMove

            action = case dragState of
                DragSelectionRect -> E.ExtendSelectionRect mouseX mouseY
                DragSelected DragMove -> E.MoveSelected mouseX mouseY
                DragSelected DragRotate -> E.DragRotateSelected mouseX mouseY

        let result = E.dispatchAction appState { ST.actionState = Drag dragState } action
        updateAppState result

alignRadiusesToMin :: MonadState AppState m => m ()
alignRadiusesToMin = do
    appState <- get
    E.dispatchAction appState E.LevelSelectedRadiusesToMin & updateAppState

alignRadiusesToMax :: MonadState AppState m => m ()
alignRadiusesToMax = do
    appState <- get
    E.dispatchAction appState E.LevelSelectedRadiusesToMax & updateAppState

setViewScale :: MonadState AppState m => Double -> m ()
setViewScale scaleFactor = modify (\s -> s { ST.viewScale = scaleFactor })

setViewTranslate :: MonadState AppState m => Double -> Double -> m ()
setViewTranslate trX trY = modify (\s -> s { ST.translateX = trX, ST.translateY = trY })

menuSave :: (MonadState AppState m, MonadIO m) => Gtk.Window -> m (Maybe Text)
menuSave w = do
    filename <- gets ST.currentFileName
    case filename of
        Nothing -> void $ menuSaveAs w
        Just f  -> writeAnimationToFile f
    return filename

menuSaveAs :: (MonadState AppState m, MonadIO m) => Gtk.Window -> m (Maybe Text)
menuSaveAs w = do
    filename <- saveFileChooserDialog w
    forM_ filename writeAnimationToFile
    return filename

writeAnimationToFile :: (MonadState AppState m, MonadIO m) => Text -> m ()
writeAnimationToFile filename = do
    json <- gets $ encode . ST.animation
    liftIO $ BS.writeFile (unpack filename) json
    modify (\s -> s { ST.currentFileName = Just filename })

menuOpen :: (MonadState AppState m, MonadIO m) => Gtk.Window -> m (Maybe Text)
menuOpen w = do
    state <- get
    filename <- openFileChooserDialog w

    join <$> forM filename (\f -> do
        json <- liftIO $ BS.readFile (unpack f)
        forM (decode json) (\a -> do
            put state { ST.animation = a, ST.currentFileName = Just f }
            return f))


saveFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe Text)
saveFileChooserDialog = fileChooserDialog Gtk.FileChooserActionSave

openFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe Text)
openFileChooserDialog = fileChooserDialog Gtk.FileChooserActionOpen

fileChooserDialog :: MonadIO m => Gtk.FileChooserAction -> Gtk.Window -> m (Maybe Text)
fileChooserDialog actionType mainWindow = do
    dlg <- new Gtk.FileChooserDialog [ #title := "Save animation"
                                     , #action := actionType
                                     ]

    Gtk.windowSetTransientFor dlg $ Just mainWindow

    _ <- Gtk.dialogAddButton dlg
            (case actionType of
                Gtk.FileChooserActionSave -> "gtk-save"
                _                         -> "gtk-open")
            $ (toEnum . fromEnum) Gtk.ResponseTypeAccept

    _ <- Gtk.dialogAddButton dlg "gtk-cancel" $ (toEnum . fromEnum) Gtk.ResponseTypeCancel

    Gtk.widgetShow dlg
    result <- fromIntegral <$> Gtk.dialogRun dlg
    filename <- Gtk.fileChooserGetFilename dlg

    let resultFilename =
            case toEnum result of
                Gtk.ResponseTypeAccept -> pack <$> filename
                _                      -> Nothing


    Gtk.widgetDestroy dlg
    return resultFilename
