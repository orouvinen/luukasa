{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Luukasa.Event.Handler.Editor where

import           Control.Monad                     (foldM, unless, void, when)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.State               (MonadState, gets, modify)
import           Data.Foldable                     (forM_)
import           Data.Function                     ((&))
import           Data.GI.Base                      (AttrOp ((:=)), new)
import           Data.Int                          (Int32)
import           Data.Map                          ((!))
import qualified Data.Map                          as Map
import           Data.Text                         (Text, pack, unpack)
import qualified GI.Gdk                            as Gdk
import qualified GI.Gtk                            as Gtk
import qualified Luukasa.Animation                 as A
import           Luukasa.AnimatorState             (ActionState (..),
                                                    AnimatorState,
                                                    DragMode (..),
                                                    DragState (..))

import           Data.Functor                      ((<&>))
import qualified Luukasa.AnimatorState             as ST
import           Luukasa.AppState                  (AppState)
import qualified Luukasa.AppState                  as App
import qualified Luukasa.Body                      as B
import           Luukasa.Common                    (ErrorMessage)
import           Luukasa.EditorAction              as E (Action (..),
                                                         SelectMode (..),
                                                         dispatchAction)
import           Luukasa.Event.Handler.EventResult (EventResult, toEventResult,
                                                    updateAnimatorState)
import           Luukasa.Event.JsonFileIO          (JsonFileIO (..))
import           Luukasa.Event.Keyboard            (KeyEvent (..))
import           Luukasa.Event.Mouse               (MouseEvent, clickModifiers,
                                                    clickPos,
                                                    getScrollDirection,
                                                    motionModifiers, motionPos)
import           Luukasa.Event.Ui.UiElement
import           Luukasa.Joint                     (Joint, JointId,
                                                    JointLockMode (..))
import qualified Luukasa.Joint                     as J

jointDragLockModifier, selectToggleModifier :: Gdk.ModifierType
jointDragLockModifier = Gdk.ModifierTypeShiftMask
selectToggleModifier = Gdk.ModifierTypeControlMask

selectLockMode :: MonadState AppState m => JointLockMode -> m ()
selectLockMode lockMode = modify (\s -> s { App.animatorState = (App.animatorState s) { ST.jointLockMode = lockMode } })

canvasPrimaryMouseButtonClick :: (MonadState AppState m, MouseEvent m) => Gdk.EventButton -> m ()
canvasPrimaryMouseButtonClick e = do
    s <- gets App.animatorState

    (x', y') <- clickPos e
    let x = truncate x'
        y = truncate y'

    toggleSelect <- elem selectToggleModifier <$> clickModifiers e

    let dispatch = dispatchAction s
        result = case ST.actionState s of
            PlacingNewJoint -> dispatch $ E.CreateJoint x y
            Idle            -> dispatch $ E.TrySelect x y $ if toggleSelect then Toggle else Set
            _               -> Right s

    updateAnimatorState result

canvasPrimaryMouseButtonRelease :: MonadState AppState m => Gdk.EventButton -> m ()
canvasPrimaryMouseButtonRelease _ = do
    isPlaybackOn <- gets (ST.isPlaybackOn . App.animatorState)
    unless isPlaybackOn $ modify (\s' -> s' { App.animatorState = (App.animatorState s') { ST.actionState = Idle } })

canvasKeyPress :: (MonadState AppState m, KeyEvent m) => Gdk.EventKey -> m (EventResult ())
canvasKeyPress eventKey = do
    s <- gets App.animatorState
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

    updateAnimatorState result
    toEventResult result ()

scrollWheelScaleStep :: Double
scrollWheelScaleStep = 0.1

canvasScrollWheel :: (MonadState AppState m, MouseEvent m) => Gdk.EventScroll -> m ()
canvasScrollWheel eventScroll = do
    scrollDirection <- getScrollDirection eventScroll

    let scaleChange =
            if scrollDirection == Gdk.ScrollDirectionDown
            then -scrollWheelScaleStep
            else scrollWheelScaleStep

    s <- gets App.animatorState
    App.putAnimatorState s { ST.viewScale = ST.viewScale s + scaleChange }


canvasMouseMotion :: (MonadState AppState m, MouseEvent m) => Gdk.EventMotion -> m ()
canvasMouseMotion e = do
    appState <- gets App.animatorState

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
        updateAnimatorState result

alignRadiusesToMin :: MonadState AppState m => m ()
alignRadiusesToMin = do
    s <- gets App.animatorState
    E.dispatchAction s E.LevelSelectedRadiusesToMin & updateAnimatorState

alignRadiusesToMax :: MonadState AppState m => m ()
alignRadiusesToMax = do
    s <- gets App.animatorState
    E.dispatchAction s E.LevelSelectedRadiusesToMax & updateAnimatorState

setViewScale ::  MonadState AppState m => Double -> m ()
setViewScale scaleFactor = do
    s <- gets App.animatorState
    App.putAnimatorState s { ST.viewScale = scaleFactor }

setViewTranslate ::  MonadState AppState m => Double -> Double -> m ()
setViewTranslate trX trY = do
    s <- gets App.animatorState
    App.putAnimatorState s { ST.translateX = trX, ST.translateY = trY }

modifyAnimationJointWith :: MonadState AppState m => JointId -> (Joint -> Joint) -> m ()
modifyAnimationJointWith jointId f = do
    s <- gets App.animatorState
    E.dispatchAction s (E.ApplyToAnimationJointWithId jointId f) & updateAnimatorState

selectJoint :: MonadState AppState m => JointId -> m ()
selectJoint jointId = do
    s <- gets App.animatorState
    App.putAnimatorState s { ST.selectedJointIds = [jointId] }

updateJointList
    :: (MonadState AppState m, HasUiListStore m)
    => Gtk.ListStore
    -> Map.Map JointId [Gtk.GValue]
    -> m ()
updateJointList jointListStore jointValues = do
    s <- gets App.animatorState
    clearListStore jointListStore

    let joints = B.toJointList $ A.currentFrameData (ST.animation s)

    iterLookup <- foldM (\lkup j -> do
            iter <- insertListRow jointListStore (jointValues ! J.jointId j)
            iterAsString <- getIterAsString jointListStore iter
            return $ Map.insert iterAsString (J.jointId j) lkup)
        (Map.empty :: Map.Map Text JointId)
        joints

    App.putAnimatorState s { ST.jointIterLookup = iterLookup }

{- TODO: this is awful in few different ways.
Of course, everything has potential for refactoring, but oh boy
has this some priority..

Anyway, the mission here is to update
    1. joint value on UI (visual)
    2. joint data in AppState (factual)
-}
setJointAttribute
    :: (MonadState AppState m, HasUiListStore m)
    => Gtk.ListStore -- ^ list store to update
    -> Text          -- ^ TreeView path as given by Gdk
    -> Gtk.GValue    -- ^ Value to set to desired cell
    -> Int32         -- ^ Column number for the data
    -> (Joint -> Joint) -- ^ Function to update joint data
    -> m ()
setJointAttribute jointListStore path cellVal colNum updateJoint = do
    mbIter <- getIterFromString jointListStore path
    case mbIter of
        Nothing -> return ()
        Just iter -> do
            listStoreSetValue jointListStore iter colNum cellVal

            appState <- gets App.animatorState
            let jointIterLookup = ST.jointIterLookup appState
                jointId = jointIterLookup ! path
            E.dispatchAction appState (E.ApplyToAnimationJointWithId jointId updateJoint) & updateAnimatorState

menuSave :: (MonadState AppState m, JsonFileIO AnimatorState m) => Gtk.Window -> m (Maybe Text)
menuSave w = do
    filename <- gets App.animatorState <&> ST.currentFileName
    case filename of
        Nothing -> void $ menuSaveAs w
        Just f  -> writeAnimationToFile f
    return filename

menuSaveAs :: (MonadState AppState m, JsonFileIO AnimatorState m) => Gtk.Window -> m (Maybe Text)
menuSaveAs w = do
    filename <- saveFileChooserDialog w
    forM_ filename writeAnimationToFile
    return filename

writeAnimationToFile :: (MonadState AppState m, JsonFileIO AnimatorState m) => Text -> m ()
writeAnimationToFile filename = do
    s <- gets App.animatorState
    writeJson (unpack filename) s
    App.putAnimatorState s { ST.currentFileName = Just filename }

{- If JSON decoding fails, Left "error message" will be returned just the way it comes from Aeson.
If nothing goes wrong, then we might have a filename (Right Maybe "filename") if the
file chooser dialog wasn't canceled.
-}
menuOpen :: (MonadState AppState m, JsonFileIO AnimatorState m) => Gtk.Window -> m (Either ErrorMessage (Maybe Text))
menuOpen w = do
    filename <- openFileChooserDialog w

    case filename of
        Nothing -> return $ Right Nothing
        Just f -> do
            decoded <- readJson (unpack f)
            case decoded of
                Left err -> return $ Left (pack err)
                Right animatorState -> do
                    App.putAnimatorState animatorState
                    return $ Right (Just f)


saveFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe Text)
saveFileChooserDialog = fileChooserDialog Gtk.FileChooserActionSave

openFileChooserDialog :: MonadIO m => Gtk.Window -> m (Maybe Text)
openFileChooserDialog = fileChooserDialog Gtk.FileChooserActionOpen

fileChooserDialog :: MonadIO m => Gtk.FileChooserAction -> Gtk.Window -> m (Maybe Text)
fileChooserDialog actionType mainWindow = do
    dlg <- new Gtk.FileChooserDialog
        [ #title :=
            if actionType == Gtk.FileChooserActionSave
                then "Save animation"
                else "Open animation"
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
