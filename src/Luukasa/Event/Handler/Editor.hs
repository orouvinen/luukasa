{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Luukasa.Event.Handler.Editor where

import           Control.Monad                     (unless, when)
import           Control.Monad.State               (MonadState, gets, modify)
import           Data.Function                     ((&))
import qualified GI.Gdk                            as Gdk
import           Luukasa.AnimatorState             (ActionState (..),
                                                    DragMode (..),
                                                    DragState (..))

import qualified Luukasa.AnimatorState             as ST
import           Luukasa.AppState                  (AppState)
import qualified Luukasa.AppState                  as App
import           Luukasa.EditorAction              as E (Action (..),
                                                         SelectMode (..),
                                                         dispatchAction)
import           Luukasa.Event.Handler.EventResult (EventResult, toEventResult,
                                                    updateAnimatorState)
import           Luukasa.Event.Keyboard            (KeyEvent (..))
import           Luukasa.Event.Mouse               (MouseEvent, clickModifiers,
                                                    clickPos,
                                                    getScrollDirection,
                                                    motionModifiers, motionPos)
import           Luukasa.Joint                     (Joint, JointId,
                                                    JointLockMode (..))

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

        E.dispatchAction appState { ST.actionState = Drag dragState } action
            & updateAnimatorState

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
