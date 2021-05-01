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
import           Luukasa.Event.Keyboard
import           Luukasa.Event.Mouse       (HasMouseEvent, clickModifiers,
                                            clickPos, getScrollDirection,
                                            motionModifiers, motionPos)
import qualified Luukasa.EventHandler      as EV
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
    -- eventPos e = (,) <$> (e `Gdk.get` #x) <*> (e `Gdk.get` #y)
    -- clickPos e = (,) <$> (e `Gdk.get` #x) <*> (e `Gdk.get` #y)
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

buildUi :: IORef AppState -> IO ()
buildUi stateRef = do
    let runEventHandler = runEvent stateRef
    builder <- GO.builderNew
    GO.builderAddFromFile builder "ui/main.glade"

    window <- GO.builderGetObject builder "mainWindow" >>= Gtk.unsafeCastTo Gtk.Window . fromJust
    Gtk.onWidgetDestroy window Gtk.mainQuit

    canvas <- GO.builderGetObject builder "mainCanvas" >>= Gtk.unsafeCastTo Gtk.DrawingArea . fromJust
    fileOpen <- GO.builderGetObject builder "fileOpen" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileSave <- GO.builderGetObject builder "fileSave" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileQuit <- GO.builderGetObject builder "fileQuit" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust

    statusBar <- GO.builderGetObject builder "statusBarLabel" >>= Gtk.unsafeCastTo Gtk.Label . fromJust
    Gtk.labelSetLabel statusBar "Luukasa started"

    btnPlayback <- GO.builderGetObject builder "btnPlayback" >>= Gtk.unsafeCastTo Gtk.Button . fromJust
    Gtk.onButtonClicked btnPlayback $ playbackHandler stateRef canvas btnPlayback

    -- Event handlers
    _ <- Gtk.onWidgetDraw canvas $ renderWithContext (Render.render stateRef)

    -- Event handling for drawing area
    _ <- Gtk.onWidgetScrollEvent canvas $ \ev -> do
        runEventHandler $ EV.canvasScrollWheel ev
        Gtk.widgetQueueDraw canvas
        return True

    Gtk.widgetAddEvents canvas
        [ Gdk.EventMaskButtonPressMask
        , Gdk.EventMaskButtonReleaseMask
        , Gdk.EventMaskPointerMotionMask
        , Gdk.EventMaskPointerMotionHintMask
        , Gdk.EventMaskScrollMask
        ]

    _ <- Gtk.onWidgetButtonPressEvent canvas $ \ev -> do
        button <- fromIntegral <$> Gdk.getEventButtonButton ev
        case button of
            Gdk.BUTTON_PRIMARY -> do
                                    res <- runEventHandler $ EV.canvasPrimaryMouseButtonClick ev
                                    -- TODO handle result
                                    return ()
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
        runEventHandler $ EV.canvasKeyPress ev
        key <- Gdk.getEventKeyKeyval ev

        when (key == Gdk.KEY_space) $ playbackHandler stateRef canvas btnPlayback

        Gtk.widgetQueueDraw canvas
        return True

    _ <- Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
        res <- runEventHandler $ EV.canvasMouseMotion ev
        -- TODO: handle result
        Gtk.widgetQueueDraw canvas
        return True

    -- Menu item actions
    _ <- Gtk.onMenuItemActivate fileQuit Gtk.mainQuit
    _ <- Gtk.onMenuItemActivate fileSave $ runEvent stateRef (EV.menuSave window)
    _ <- Gtk.onMenuItemActivate fileOpen $ runEvent stateRef (EV.menuOpen window)

    -- View local coordinate [0,0] at center of the canvas
    Gtk.onWidgetSizeAllocate canvas $ \_ -> do
        newWidth <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
        newHeight <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
        _ <- runEventHandler $ EV.setViewTranslate (newWidth / 2) (newHeight / 2)
        return ()

    Gtk.widgetShowAll window

    return ()


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
