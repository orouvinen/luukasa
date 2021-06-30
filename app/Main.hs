{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Exception                 (IOException, catch)
import           Control.Monad                     (foldM, forM_, unless, void,
                                                    when)
import           Data.Foldable                     (Foldable (toList))
import           Data.IORef                        (IORef, newIORef, readIORef,
                                                    writeIORef)
import           Data.Map                          (Map, (!))
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as TextIO (putStrLn)
import           GI.Cairo.Render.Connector         (renderWithContext)
import qualified GI.Gdk                            as Gdk
import qualified GI.Gtk                            as Gtk
import qualified GI.Gtk.Objects                    as GO
import           LimitedRange                      (lower, setLower, setUpper,
                                                    upper)
import           Luukasa.AnimatorState             (ActionState (..))
import qualified Luukasa.AnimatorState             as ST
import           Luukasa.AppState                  (AppState)
import qualified Luukasa.AppState                  as App
import qualified Luukasa.Body                      as B
import           Luukasa.Common                    (ErrorMessage)
import           Luukasa.Event.EventM              (EventM, runEvent)
import qualified Luukasa.Event.Handler             as EV
import qualified Luukasa.Event.Handler.SeqGenModal as SeqGenModal
import qualified Luukasa.Joint                     as J
import qualified Luukasa.Render                    as Render (render)
import qualified Luukasa.UiState                   as UI
import           Units                             (getDegrees, mkDegrees)

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    buildUi
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
    joints <- toList . B.root . ST.visibleBody . App.animatorState <$> readIORef stateRef
    foldM (\lkup j -> do
            jointGValues <- jointToGValues j
            return $ Map.insert (J.jointId j) jointGValues lkup)
        Map.empty
        joints


buildUi :: IO ()
buildUi = do
    stateRef <- newIORef App.initialAppState

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

    -- Dialog for sequence generation command
    dlgSeqGen <- GO.builderGetObject builder "sequenceGenerate" >>= Gtk.unsafeCastTo Gtk.Dialog . fromJust
    Gtk.windowSetTransientFor dlgSeqGen $ Just window

    -- Menu item for sequence generation
    editSeqGen <- GO.builderGetObject builder "editSeqGen" >>= Gtk.unsafeCastTo Gtk.MenuItem . fromJust

    -- Event runners
    let runEventHandler = runEvent stateRef

        runEventHandlerWithResult = \onError handler -> do
            Gtk.labelSetText statusBar ""
            runEventWithResult stateRef onError handler

        updateJointList = do
            jointGValues <- jointsAsGValues stateRef
            runEventHandler $ EV.updateJointList jointListStore jointGValues

    -- Seq. generation modal controls
    genStartFrame                   <- GO.builderGetObject builder "genStartFrame"  >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genEndFrame                     <- GO.builderGetObject builder "genEndFrame"    >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genTargetX                      <- GO.builderGetObject builder "genTargetX"     >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genTargetY                      <- GO.builderGetObject builder "genTargetY"     >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genTargetLocalRot               <- GO.builderGetObject builder "genTargetLocalRot" >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genTargetWorldRot               <- GO.builderGetObject builder "genTargetWorldRot" >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genTargetRotDelta               <- GO.builderGetObject builder "genTargetRotDelta"  >>= Gtk.unsafeCastTo Gtk.Entry . fromJust
    genRadioSeqTypeStill            <- GO.builderGetObject builder "radioSeqTypeStill"     >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTypeTranslate        <- GO.builderGetObject builder "radioSeqTypeTranslate" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTypeRotate           <- GO.builderGetObject builder "radioSeqTypeRotate"    >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTargetPos            <- GO.builderGetObject builder "radioSeqTargetPosition" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTargetLocalRot       <- GO.builderGetObject builder "radioSeqTargetLocalRot" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTargetWorldRot       <- GO.builderGetObject builder "radioSeqTargetWorldRot" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqTargetRotDelta       <- GO.builderGetObject builder "radioSeqTargetRotDelta" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqAccelTypePerFrame    <- GO.builderGetObject builder "radioGenAccelPerFrame"  >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust
    genRadioSeqAccelTypePerSecond   <- GO.builderGetObject builder "radioGenAccelPerSecond" >>= Gtk.unsafeCastTo Gtk.RadioButton . fromJust

    btnSeqGenOk <- GO.builderGetObject builder "genOk" >>= Gtk.unsafeCastTo Gtk.Button . fromJust
    btnSeqGenCancel <- GO.builderGetObject builder "genCancel" >>= Gtk.unsafeCastTo Gtk.Button . fromJust

    -- (numeric) entries
    _ <- Gtk.onEditableChanged genStartFrame $ runEventHandler $ SeqGenModal.entryValueUpdated genStartFrame parseInt (\v s -> s { UI.startFrame = v })
    _ <- Gtk.onEditableChanged genEndFrame $ runEventHandler $ SeqGenModal.entryValueUpdated genStartFrame parseInt (\v s -> s { UI.endFrame = v })
    _ <- Gtk.onEditableChanged genTargetX $ runEventHandler $ SeqGenModal.entryValueUpdated genTargetX parseDouble (\x s -> s { UI.targetX = x })
    _ <- Gtk.onEditableChanged genTargetY $ runEventHandler $ SeqGenModal.entryValueUpdated genTargetY parseDouble (\y s -> s { UI.targetY = y })
    _ <- Gtk.onEditableChanged genTargetLocalRot $ runEventHandler $ SeqGenModal.entryValueUpdated genTargetY parseDouble (\v s -> s { UI.targetLocalRot = v })
    _ <- Gtk.onEditableChanged genTargetWorldRot $ runEventHandler $ SeqGenModal.entryValueUpdated genTargetY parseDouble (\v s -> s { UI.targetWorldRot = v })
    _ <- Gtk.onEditableChanged genTargetRotDelta $ runEventHandler $ SeqGenModal.entryValueUpdated genTargetY parseDouble (\v s -> s { UI.targetRotDelta = v })

    -- Sequence type selection radio buttons
    _ <- Gtk.onButtonClicked genRadioSeqTypeStill $ runEventHandler $ SeqGenModal.setSeqType SeqGenModal.Still
    _ <- Gtk.onButtonClicked genRadioSeqTypeTranslate $ runEventHandler $ SeqGenModal.setSeqType SeqGenModal.Translate
    _ <- Gtk.onButtonClicked genRadioSeqTypeRotate $ runEventHandler $ SeqGenModal.setSeqType SeqGenModal.Rotate

    -- Sequence accel type radio buttons
    _ <- Gtk.onButtonClicked genRadioSeqAccelTypePerFrame $ runEventHandler $ SeqGenModal.setAccelType SeqGenModal.AccelPerFrame
    _ <- Gtk.onButtonClicked genRadioSeqAccelTypePerSecond $ runEventHandler $ SeqGenModal.setAccelType SeqGenModal.AccelPerSecond

    -- Sequence target type radio buttons
    _ <- Gtk.onButtonClicked genRadioSeqTargetPos $ runEventHandler $ SeqGenModal.setTargetType SeqGenModal.TargetPos
    _ <- Gtk.onButtonClicked genRadioSeqTargetLocalRot $ runEventHandler $ SeqGenModal.setTargetType SeqGenModal.TargetLocalRot
    _ <- Gtk.onButtonClicked genRadioSeqTargetWorldRot $ runEventHandler $ SeqGenModal.setTargetType SeqGenModal.TargetWorldRot
    _ <- Gtk.onButtonClicked genRadioSeqTargetRotDelta $ runEventHandler $ SeqGenModal.setTargetType SeqGenModal.TargetRotateDelta

    -- Event handlers

    -- joint name edited
    _ <- Gtk.onCellRendererTextEdited jointNameCell $ \path enteredText -> do
            newVal <- Gtk.toGValue (Just enteredText)
            runEventHandler $ EV.setJointAttribute jointListStore path newVal 0 (\j -> j { J.jointName = Just enteredText })
            s <- readIORef stateRef
            writeIORef stateRef $ s { App.uiState = (App.uiState s) { UI.isCellEditActive = False } }

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
        writeIORef stateRef s { App.uiState = (App.uiState s) { UI.isCellEditActive = False } }

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
        writeIORef stateRef s { App.uiState = (App.uiState s) { UI.isCellEditActive = False } }


    -- Set editing flag when any of the joint list columns are getting edited
    forM_
        [jointNameCell, jointRotMinCell, jointRotMaxCell]
        (\cell -> do
            void $ Gtk.onCellRendererEditingStarted cell $ \_ _ -> do
                s <- readIORef stateRef
                writeIORef stateRef s { App.uiState = (App.uiState s) { UI.isCellEditActive = True } })

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
        jointIterLookup <- ST.jointIterLookup . App.animatorState <$> readIORef stateRef
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
        isCellEditing <- UI.isCellEditActive . App.uiState <$> readIORef stateRef
        unless isCellEditing updateJointList
        return True

    _ <- Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
        runEventHandler $ EV.canvasMouseMotion ev
        Gtk.widgetQueueDraw canvas
        return True

    -- Menu item actions

    -- File menu
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

    -- Edit menu
    _ <- Gtk.onMenuItemActivate editSeqGen $ do
        res <- Gtk.dialogRun dlgSeqGen
        Gtk.widgetHide dlgSeqGen
        return ()


    -- View local coordinate [0,0] at center of the canvas
    _ <- Gtk.onWidgetSizeAllocate canvas $ \_ -> do
        newWidth <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
        newHeight <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
        _ <- runEventHandler $ EV.setViewTranslate (newWidth / 2) (newHeight / 2)
        return ()

    Gtk.widgetShowAll window
    updateJointList
    Gtk.labelSetText statusBar "Luukasa started"

handleIOException :: IOException -> IO ()
handleIOException = print

showFileResult :: T.Text -> Gtk.Label -> Maybe T.Text -> IO ()
showFileResult _ _ Nothing = return ()
showFileResult verb label (Just filename) = Gtk.labelSetText label $ filename <> " " <> verb


playbackHandler :: IORef AppState -> Gtk.DrawingArea -> Gtk.Button -> IO ()
playbackHandler stateRef canvas btnPlayback = do
    let runEventHandler = runEvent stateRef

    animatorState <- App.animatorState <$> readIORef stateRef

    case ST.actionState animatorState of
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

{- Something needs to be done to these. And they don't belong here either. But for now I just want things to work. Hence these parse-things. -}
parseInt :: T.Text -> Maybe Int
parseInt x =
    let parsed = reads (T.unpack x) :: [(Int, String)]
    in if null parsed
        then Nothing
        else Just $ fst $ head parsed

parseDouble :: T.Text -> Maybe Double
parseDouble x =
    let parsed = reads (T.unpack x) :: [(Double, String)]
    in if null parsed
        then Nothing
        else Just $ fst $ head parsed
