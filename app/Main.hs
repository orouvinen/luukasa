{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.GI.Base
import           Data.IORef
import           GI.Cairo.Render.Connector (renderWithContext)
import           GI.Gdk                    as Gdk
import qualified GI.Gtk                    as Gtk

import           Luukasa.AppState
import qualified Luukasa.Render            as Render
import qualified Luukasa.UiEventHandler    as EV

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

main :: IO ()
main = do
    _ <- Gtk.init Nothing

    stateRef <- newIORef initialState
    buildUi stateRef
    Gtk.main


buildUi :: IORef AppState -> IO ()
buildUi state = do
    -- Create main window
    window <- new Gtk.Window [#title := "luukasa-dev"]
    Gtk.widgetSetAppPaintable window True
    _ <- on window #destroy Gtk.mainQuit

    -- Create layout grid
    grid <- Gtk.gridNew

    -- Create drawing area for Cairo rendering
    canvas <- Gtk.drawingAreaNew
    _ <- Gtk.onWidgetDraw canvas $ renderWithContext (Render.render state)
    Gtk.widgetSetSizeRequest canvas (fromIntegral windowWidth) (fromIntegral windowHeight)

    -- Event handling for drawing area
    _ <- Gtk.onWidgetScrollEvent canvas $ \ev -> do
        _ <- EV.canvasScrollWheel state ev
        Gtk.widgetQueueDrawArea canvas 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight)
        return True


    Gtk.widgetAddEvents canvas
        [ Gdk.EventMaskButtonPressMask
        , Gdk.EventMaskButtonReleaseMask
        , Gdk.EventMaskPointerMotionMask
        , Gdk.EventMaskPointerMotionHintMask
        , Gdk.EventMaskScrollMask
        ]

    _ <- Gtk.onWidgetButtonPressEvent canvas $ \ev -> do
        button <- fromIntegral <$> ev `Gdk.get` #button

        case button of
            Gdk.BUTTON_PRIMARY   -> EV.canvasPrimaryMouseButtonClick state ev
            Gdk.BUTTON_SECONDARY -> return ()
            Gdk.BUTTON_MIDDLE    -> return ()

        Gtk.widgetQueueDrawArea canvas 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight)
        return True

    _ <- Gtk.onWidgetButtonReleaseEvent canvas $ \ev -> do
        button <- fromIntegral <$> ev `Gdk.get` #button

        case button of
            Gdk.BUTTON_PRIMARY -> EV.canvasPrimaryMouseButtonRelease state ev
            Gdk.BUTTON_SECONDARY -> return ()
            Gdk.BUTTON_MIDDLE -> return ()

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
        EV.canvasKeyPress state ev
        --Gtk.widgetQueueDrawArea canvas 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight)
        Gtk.widgetQueueDraw canvas
        return False

    _ <- Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
        EV.canvasMouseMotion state ev
        Gtk.widgetQueueDraw canvas
        return True

    -- Put all the parts together
    Gtk.gridAttach grid canvas 0 1 1 1
    Gtk.containerAdd window grid
    Gtk.windowSetPosition window Gtk.WindowPositionCenter

    #showAll window

    {-
        Set up initial view translation so that coordinate (0, 0)
        is rendered at center of the drawing area.
    -}
    width' <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
    height' <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
    _ <- EV.setViewTranslate state (width' / 2) (height' / 2)

    return ()

