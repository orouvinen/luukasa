{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk                    as Gdk
import qualified GI.Gtk                    as Gtk
import qualified GI.Gtk.Objects            as GO

import           Control.Monad             (when, (>=>))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (MonadReader, ReaderT, ask,
                                            runReaderT)
import           Data.Maybe                (fromJust)
import           Luukasa.AppState          (ActionState (..), AppState,
                                            HasAppState, actionState, get,
                                            initialState, put)
import           Luukasa.Common            (ErrorMessage)
import           Luukasa.Event.Keyboard
import           Luukasa.Event.Mouse       (HasMouseEvent, clickModifiers,
                                            clickPos, getScrollDirection,
                                            motionModifiers, motionPos)
import qualified Luukasa.EventHandler      as EV
import qualified Luukasa.Joint
import qualified Luukasa.Render            as Render (render)

newtype EventM a = EventM { runEventM :: ReaderT (IORef AppState) IO a }
    deriving (Functor, Applicative, Monad, MonadReader (IORef AppState), MonadIO)

instance HasAppState EventM where
    get = ask >>= liftIO . readIORef
    put s = ask >>= \stateRef -> liftIO $ writeIORef stateRef s

instance HasKeyEvent EventM where
    getKey = Gdk.getEventKeyKeyval >=> Gdk.keyvalToUpper

instance HasMouseEvent EventM where
    getScrollDirection = Gdk.getEventScrollDirection
    clickPos e = (,) <$> Gdk.getEventButtonX e <*> Gdk.getEventButtonY e
    motionPos e = (,) <$> Gdk.getEventMotionX e <*> Gdk.getEventMotionY e
    clickModifiers = Gdk.getEventButtonState
    motionModifiers = Gdk.getEventMotionState

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    newIORef initialState >>= buildUi
    Gtk.main

runEvent :: IORef AppState -> EventM a -> IO a
runEvent stateRef handler = runReaderT (runEventM handler) stateRef

runEventWithResult
    :: IORef AppState
    -> (ErrorMessage -> IO ())
    -> EventM (Either ErrorMessage a)
    -> IO (Either ErrorMessage a)
runEventWithResult stateRef onError eventHandler = do
    result <- runEvent stateRef eventHandler
    case result of
        Left err -> onError err
        Right _  -> return ()

    return result

buildUi :: IORef AppState -> IO ()
buildUi stateRef = do
    builder <- GO.builderNew
    _ <- GO.builderAddFromFile builder "ui/main.glade"

    window <- GO.builderGetObject builder "mainWindow" >>= Gtk.unsafeCastTo Gtk.Window . fromJust
    canvas <- GO.builderGetObject builder "mainCanvas" >>= Gtk.unsafeCastTo Gtk.DrawingArea . fromJust

    -- Menu items
    fileOpen <- GO.builderGetObject builder "fileOpen" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileSave <- GO.builderGetObject builder "fileSave" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileQuit <- GO.builderGetObject builder "fileQuit" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust

    -- Button bar buttons
    btnPlayback <- GO.builderGetObject builder "btnPlayback" >>= Gtk.unsafeCastTo Gtk.Button . fromJust

    -- Joint lock mode radio buttons
    radioLockModeNoLock <- GO.builderGetObject builder "radioLockModeNoLock" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    radioLockModeDrag <- GO.builderGetObject builder "radioLockModeDrag" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    radioLockModeRotate <- GO.builderGetObject builder "radioLockModeRotate" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust

    -- Bottom grid items
    statusBar <- GO.builderGetObject builder "statusBarLabel" >>= Gtk.unsafeCastTo Gtk.Label . fromJust
    Gtk.labelSetText statusBar "Luukasa started"

    -- Event runners
    let runEventHandler = runEvent stateRef
        runEventHandlerWithResult = \onError handler -> Gtk.labelSetText statusBar "" >> runEventWithResult stateRef onError handler

    -- Event handlers
    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    _ <- Gtk.onButtonClicked radioLockModeNoLock $ runEventHandler $ EV.selectLockMode Luukasa.Joint.NoLock
    _ <- Gtk.onButtonClicked radioLockModeDrag $ runEventHandler $ EV.selectLockMode Luukasa.Joint.Drag
    _ <- Gtk.onButtonClicked radioLockModeRotate $ runEventHandler $ EV.selectLockMode Luukasa.Joint.Rotate

    _ <- Gtk.onButtonClicked btnPlayback $ playbackHandler stateRef canvas btnPlayback

    _ <- Gtk.onWidgetDraw canvas $ renderWithContext (Render.render stateRef)

    -- Event handling for drawing area
    _ <- Gtk.onWidgetScrollEvent canvas $ \ev -> do
        runEventHandler $ EV.canvasScrollWheel ev
        Gtk.widgetQueueDraw canvas
        return True

    _ <- Gtk.onWidgetButtonPressEvent canvas $ \ev -> do
        button <- fromIntegral <$> Gdk.getEventButtonButton ev
        case button of
            Gdk.BUTTON_PRIMARY -> runEventHandler $ EV.canvasPrimaryMouseButtonClick ev
            _                  -> return ()

        Gtk.widgetQueueDraw canvas
        return True

    _ <- Gtk.onWidgetButtonReleaseEvent canvas $ \ev -> do
        button <- fromIntegral <$> Gdk.getEventButtonButton ev

        case button of
            Gdk.BUTTON_PRIMARY -> runEventHandler $ EV.canvasPrimaryMouseButtonRelease ev
            _                  -> return ()

        Gtk.widgetQueueDraw canvas
        return True
    {- KeyEvent handler directly on `window`:

    " To receive mouse events on a drawing area, you will need to enable them with Widget.addEvents.
        To receive keyboard events, you will need to set the “can-focus” property on the drawing area,
        and you should probably draw some user-visible indication that the drawing area is focused.
        Use Widget.hasFocus in your expose event handler to decide whether to draw the focus indicator.
        See gtk_render_focus() for one way to draw focus. "

    source: https://gtk-d.dpldocs.info/gtk.DrawingArea.DrawingArea.html
    -}
    _ <- Gtk.onWidgetKeyPressEvent window $ \ev -> do
        _ <- runEventHandlerWithResult (Gtk.labelSetText statusBar) (EV.canvasKeyPress ev)

        -- For now, handle playback event here directly
        key <- Gdk.getEventKeyKeyval ev
        when (key == Gdk.KEY_space) $ playbackHandler stateRef canvas btnPlayback

        Gtk.widgetQueueDraw canvas
        return True

    _ <- Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
        runEventHandler $ EV.canvasMouseMotion ev
        Gtk.widgetQueueDraw canvas
        return True

    -- Menu item actions
    _ <- Gtk.onMenuItemActivate fileQuit Gtk.mainQuit
    _ <- Gtk.onMenuItemActivate fileSave $ runEventHandler (EV.menuSave window) >> Gtk.labelSetText statusBar "File saved."
    _ <- Gtk.onMenuItemActivate fileOpen $ runEventHandler (EV.menuOpen window) >> Gtk.labelSetText statusBar "File loaded."

    -- View local coordinate [0,0] at center of the canvas
    _ <- Gtk.onWidgetSizeAllocate canvas $ \_ -> do
        newWidth <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
        newHeight <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
        _ <- runEventHandler $ EV.setViewTranslate (newWidth / 2) (newHeight / 2)
        return ()

    Gtk.widgetShowAll window


playbackHandler :: IORef AppState -> Gtk.DrawingArea -> Gtk.Button -> IO ()
playbackHandler stateRef canvas btnPlayback = do
    appState <- readIORef stateRef

    case actionState appState of
        AnimationPlayback _ -> do Gtk.buttonSetLabel btnPlayback "Play"
        Idle                -> do Gtk.buttonSetLabel btnPlayback "Stop"
        _                   -> return ()

    let runEventHandler = runEvent stateRef

    case actionState appState of
        AnimationPlayback callbackId -> do
            runEventHandler EV.stopPlayback
            Gtk.widgetRemoveTickCallback canvas callbackId

        Idle -> do
            tickCallbackId <- Gtk.widgetAddTickCallback canvas (\ _ frameClock -> do
                -- Gdk.frameClockGetFrameTime frameClock >>= runEventHandler . EV.playbackTick
                timestamp <- Gdk.frameClockGetFrameTime frameClock
                needsRedraw <- runEventHandler $ EV.playbackTick timestamp
                when needsRedraw $ Gtk.widgetQueueDraw canvas
                return True)
            {- Unrealized widgets don't have a frame clock, hence the Maybe return value and
                having to use fromJust like it was nothing. (Pun maybe intended.)
            -}
            frameClock <- fromJust <$> Gtk.widgetGetFrameClock canvas
            timestamp <- Gdk.frameClockGetFrameTime frameClock
            runEventHandler $ EV.startPlayback tickCallbackId timestamp
        _ -> return ()
