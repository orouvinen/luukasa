{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Luukasa.Event.Handler.SeqGenModal
    ( entryValueUpdated
    , setAccelType
    , setTargetType
    , runSequence
    , Gen.TargetType (..)
    , Gen.AccelType (..)
    ) where

import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.State               (MonadState, gets)
import qualified Data.Text                         as T
import qualified GI.Gtk                            as Gtk
import qualified Luukasa.AnimatorState             as ST
import           Luukasa.AppState                  (AppState)
import qualified Luukasa.AppState                  as App
import           Luukasa.Event.Handler.Common      (gtkEntryToUiState)
import           Luukasa.Event.Handler.EventResult (EventResult, toEventResult,
                                                    updateAnimatorState)
import qualified Luukasa.Generate                  as Gen
import qualified Luukasa.Joint                     as J
import qualified Luukasa.UiState                   as Ui

entryValueUpdated
    :: (MonadIO m, MonadState AppState m)
    => Gtk.Entry
    -> (T.Text -> Maybe a)
    -> (a -> Ui.SeqGenModal -> Ui.SeqGenModal)
    -> m ()
entryValueUpdated inputWidget parseValue updateModel = do
    uiState <- gets App.uiState
    uiState' <- gtkEntryToUiState inputWidget parseValue
        (\x ui -> ui { Ui.seqGenModal = updateModel x (Ui.seqGenModal ui) }) uiState
    App.putUiState uiState'

setAccelType :: MonadState AppState m => Gen.AccelType -> m ()
setAccelType accelType = do
    ui <- gets App.uiState
    let seqGen = Ui.seqGenModal ui
    App.putUiState ui { Ui.seqGenModal = seqGen { Ui.accelType = accelType } }

setTargetType :: MonadState AppState m => Gen.TargetType -> m ()
setTargetType targetType = do
    ui <- gets App.uiState
    let seqGen = Ui.seqGenModal ui
    App.putUiState ui { Ui.seqGenModal = seqGen { Ui.targetType = targetType } }

runSequence :: MonadState AppState m => m (EventResult ())
runSequence = do
    ui <- gets (Ui.seqGenModal . App.uiState)
    s <- gets App.animatorState

    let selectedJoint = ST.selectedJoint s
        seqResult =
            case selectedJoint of
                Nothing -> Left "Need exactly one selected joint"
                Just j -> do
                    {- The sequence descriptor is still lacking the jointId to operate on so
                    it's filled in here.

                        TODO: #29
                    -}
                    let selectedJointId = Gen.SeqObject $ J.jointId j
                        seqDescriptor = (Ui.seqGenModalToSequenceDescriptor ui) { Gen.seqObject = selectedJointId }

                    let animState = seqStep seqDescriptor (Ui.startFrame ui) s
                    Right animState

    updateAnimatorState seqResult
    toEventResult seqResult ()


seqStep :: Gen.GenSequence -> Int -> ST.AnimatorState -> ST.AnimatorState
seqStep gen currentFrame st =
    let
        -- firstFrame (Gen.FrameSpan x _) = x
        lastFrame (Gen.FrameSpan _ x) = x
    in if currentFrame > lastFrame (Gen.seqFrameSpan gen)
        then st
        else seqStep gen (currentFrame + 1) st




