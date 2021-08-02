{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk                    as Gdk
import qualified GI.Gtk                    as Gtk
import qualified GI.Gtk.Objects            as GO

import           Control.Exception         (IOException, catch)
import           Control.Monad             (foldM, forM_, unless, void, when)
import           Data.Foldable             (Foldable (toList))
import           Data.Map                  (Map, (!))
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TextIO (putStrLn)
import           LimitedRange              (lower, setLower, setUpper, upper)
import           Luukasa.AppState          (ActionState (..), AppState,
                                            actionState, initialState)
import qualified Luukasa.AppState          as ST
import qualified Luukasa.Body              as B
import           Luukasa.Common            (ErrorMessage)
import           Luukasa.Event.EventM      (EventM, runEvent)
import qualified Luukasa.EventHandler      as EV
import qualified Luukasa.Joint             as J
import qualified Luukasa.Render            as Render (render)
import           Units                     (getDegrees, mkDegrees)

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    newIORef initialState >>= buildUi
    Gtk.main

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

jointToGValues :: J.Joint -> IO [Gtk.GValue]
jointToGValues j = do
    rotMin <- Gtk.toGValue $ getDegrees (lower $ J.jointRotLim j)
    rotMax <- Gtk.toGValue $ getDegrees (upper $ J.jointRotLim j)
    jName <- Gtk.toGValue $ J.jointName j
    return [jName, rotMin, rotMax]

jointsAsGValues :: IORef AppState -> IO (Map J.JointId [Gtk.GValue])
jointsAsGValues stateRef = do
    joints <- toList . B.root . ST.visibleBody <$> readIORef stateRef
    foldM (\lkup j -> do
            jointGValues <- jointToGValues j
            return $ Map.insert (J.jointId j) jointGValues lkup)
        Map.empty
        joints

buildUi :: IORef AppState -> IO ()
buildUi stateRef = do
    builder <- GO.builderNew
    _ <- GO.builderAddFromFile builder "ui/main.glade"

    window <- GO.builderGetObject builder "mainWindow" >>= Gtk.unsafeCastTo Gtk.Window . fromJust
    canvas <- GO.builderGetObject builder "mainCanvas" >>= Gtk.unsafeCastTo Gtk.DrawingArea . fromJust

    -- Menu items
    fileOpen <- GO.builderGetObject builder "fileOpen" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileSave <- GO.builderGetObject builder "fileSave" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileSaveAs <- GO.builderGetObject builder "fileSaveAs" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust
    fileQuit <- GO.builderGetObject builder "fileQuit" >>= Gtk.unsafeCastTo Gtk.ImageMenuItem . fromJust

    -- Button bar buttons
    btnPlayback <- GO.builderGetObject builder "btnPlayback" >>= Gtk.unsafeCastTo Gtk.Button . fromJust

    -- Joint lock mode radio buttons
    radioLockModeNoLock <- GO.builderGetObject builder "radioLockModeNoLock" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    radioLockModeDrag <- GO.builderGetObject builder "radioLockModeDrag" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    radioLockModeRotate <- GO.builderGetObject builder "radioLockModeRotate" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust

    -- Radius align buttons
    btnAlignRadiusMin <- GO.builderGetObject builder "btnAlignRadiusMin" >>= Gtk.unsafeCastTo Gtk.Button . fromJust
    btnAlignRadiusMax <- GO.builderGetObject builder "btnAlignRadiusMax" >>= Gtk.unsafeCastTo Gtk.Button . fromJust

    -- Joints list
    jointListTreeView <- GO.builderGetObject builder "jointList" >>= Gtk.unsafeCastTo Gtk.TreeView . fromJust
    jointListStore <- GO.builderGetObject builder "jointListStore" >>= Gtk.unsafeCastTo Gtk.ListStore . fromJust
    -- Joint list columns
    jointNameCell <- GO.builderGetObject builder "colJointName" >>= Gtk.unsafeCastTo Gtk.CellRendererText . fromJust
    jointRotMinCell <- GO.builderGetObject builder "colJointRotMin" >>= Gtk.unsafeCastTo Gtk.CellRendererText . fromJust
    jointRotMaxCell <- GO.builderGetObject builder "colJointRotMax" >>= Gtk.unsafeCastTo Gtk.CellRendererText . fromJust


    -- Bottom grid items
    statusBar <- GO.builderGetObject builder "statusBarLabel" >>= Gtk.unsafeCastTo Gtk.Label . fromJust
    Gtk.labelSetText statusBar "Luukasa started"

    -- Event runners
    let runEventHandler = runEvent stateRef

        runEventHandlerWithResult = \onError handler -> do
            Gtk.labelSetText statusBar ""
            runEventWithResult stateRef onError handler

        updateJointList = do
            jointGValues <- jointsAsGValues stateRef
            runEventHandler $ EV.updateJointList jointListStore jointGValues

    -- Event handlers

    -- joint name edited
    _ <- Gtk.onCellRendererTextEdited jointNameCell $ \path enteredText -> do
            newVal <- Gtk.toGValue (Just enteredText)
            runEventHandler $ EV.setJointAttribute jointListStore path newVal 0 (\j -> j { J.jointName = Just enteredText })
            s <- readIORef stateRef
            writeIORef stateRef s { ST.isCellEditActive = False }

    -- joint rotation min. edited
    _ <- Gtk.onCellRendererTextEdited jointRotMinCell $ \path enteredText -> do
        newVal <- Gtk.toGValue (Just enteredText)
        runEventHandler $ EV.setJointAttribute jointListStore path newVal 1
            (\j ->
                let parsedInput = reads (T.unpack enteredText) :: [(Double, String)]
                in if null parsedInput
                    then j
                    else
                        let enteredNum = fst . head $ parsedInput
                            newLimit = setLower (J.jointRotLim j) (mkDegrees enteredNum)
                        in j { J.jointRotLim = newLimit })
        s <- readIORef stateRef
        writeIORef stateRef s { ST.isCellEditActive = False }

    -- joint rotation max edited
    _ <- Gtk.onCellRendererTextEdited jointRotMaxCell $ \path enteredText -> do
        newVal <- Gtk.toGValue (Just enteredText)
        runEventHandler $ EV.setJointAttribute jointListStore path newVal 2
            (\j ->
                let parsedInput = reads (T.unpack enteredText) :: [(Double, String)]
                in if null parsedInput
                    then j
                    else
                        let enteredNum = fst $ head parsedInput
                            newLimit = setUpper (J.jointRotLim j) (mkDegrees enteredNum)
                        in j { J.jointRotLim = newLimit })
        s <- readIORef stateRef
        writeIORef stateRef s { ST.isCellEditActive = False }


    -- Set editing flag when any of the joint list columns are getting edited
    forM_
        [jointNameCell, jointRotMinCell, jointRotMaxCell]
        (\cell -> do
            void $ Gtk.onCellRendererEditingStarted cell $ \_ _ -> do
                s <- readIORef stateRef
                writeIORef stateRef s { ST.isCellEditActive = True })

    _ <- Gtk.onWidgetDestroy window Gtk.mainQuit

    -- Align buttons
    _ <- Gtk.onButtonClicked btnAlignRadiusMin $ runEventHandler EV.alignRadiusesToMin >> Gtk.widgetQueueDraw canvas
    _ <- Gtk.onButtonClicked btnAlignRadiusMax $ runEventHandler EV.alignRadiusesToMax >> Gtk.widgetQueueDraw canvas

    -- Lock mode radio buttons
    _ <- Gtk.onButtonClicked radioLockModeNoLock $ runEventHandler $ EV.selectLockMode J.LockNone
    _ <- Gtk.onButtonClicked radioLockModeDrag $ runEventHandler $ EV.selectLockMode J.LockDrag
    _ <- Gtk.onButtonClicked radioLockModeRotate $ runEventHandler $ EV.selectLockMode J.LockRotate

    _ <- Gtk.onButtonClicked btnPlayback $ playbackHandler stateRef canvas btnPlayback

    _ <- Gtk.onWidgetDraw canvas $ renderWithContext (Render.render stateRef)

    _ <- Gtk.onTreeViewRowActivated jointListTreeView $ \path _ -> do
        jointIterLookup <- ST.jointIterLookup <$> readIORef stateRef
        pathString <- Gtk.treePathToString path
        -- TODO: unsafe(ish)
        let jointId = jointIterLookup ! pathString
        runEventHandler $ EV.selectJoint jointId
        Gtk.widgetQueueDraw canvas

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
        btn <- fromIntegral <$> Gdk.getEventButtonButton ev
        when (btn == Gdk.BUTTON_PRIMARY) $ do
            runEventHandler $ EV.canvasPrimaryMouseButtonRelease ev
            updateJointList
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
        return False -- Keyboard events need to be handled on some of the children too

    _ <- Gtk.onWidgetKeyReleaseEvent window  $ \_ -> do
        {- Update joint list on UI after every keypress, unless a cell value
           is currently being edited - we don't want to get in the middle of that.
        -}
        isCellEditing <- ST.isCellEditActive <$> readIORef stateRef
        unless isCellEditing updateJointList
        return True

    _ <- Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
        runEventHandler $ EV.canvasMouseMotion ev
        Gtk.widgetQueueDraw canvas
        return True

    -- Menu item actions
    _ <- Gtk.onMenuItemActivate fileQuit Gtk.mainQuit
    _ <- Gtk.onMenuItemActivate fileSaveAs $ catch
                                            (runEventHandler (EV.menuSaveAs window) >>= showFileResult "saved" statusBar)
                                            handleIOException
    _ <- Gtk.onMenuItemActivate fileSave $ catch
                                            (runEventHandler (EV.menuSave window) >>= showFileResult "saved" statusBar)
                                            handleIOException
    _ <- Gtk.onMenuItemActivate fileOpen $ catch
                                            (runEventHandler (EV.menuOpen window) >>= \case
                                                Left err -> TextIO.putStrLn err
                                                Right filename -> do
                                                    showFileResult "loaded" statusBar filename
                                                    Gtk.widgetQueueDraw canvas
                                                    updateJointList)
                                            handleIOException

    -- View local coordinate [0,0] at center of the canvas
    _ <- Gtk.onWidgetSizeAllocate canvas $ \_ -> do
        newWidth <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
        newHeight <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
        _ <- runEventHandler $ EV.setViewTranslate (newWidth / 2) (newHeight / 2)
        return ()

    Gtk.widgetShowAll window
    updateJointList

handleIOException :: IOException -> IO ()
handleIOException = print

showFileResult :: T.Text -> Gtk.Label -> Maybe T.Text -> IO ()
showFileResult _ _ Nothing = return ()
showFileResult verb label (Just filename) = Gtk.labelSetText label $ filename <> " " <> verb


playbackHandler :: IORef AppState -> Gtk.DrawingArea -> Gtk.Button -> IO ()
playbackHandler stateRef canvas btnPlayback = do
    let runEventHandler = runEvent stateRef

    appState <- readIORef stateRef

    case actionState appState of
        AnimationPlayback callbackId -> do
            Gtk.buttonSetLabel btnPlayback "Play"
            runEventHandler EV.stopPlayback
            Gtk.widgetRemoveTickCallback canvas callbackId

        Idle -> do
            Gtk.buttonSetLabel btnPlayback "Stop"
            tickCallbackId <- Gtk.widgetAddTickCallback canvas (\ _ frameClock -> do
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
