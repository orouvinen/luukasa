{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Luukasa.Event.Handler.SeqGenModal
    ( entryValueUpdated
    , setSeqType
    , setAccelType
    , setTargetType
    , Gen.SeqType (..)
    , Gen.TargetType (..)
    , Gen.AccelType (..)
    )
    where

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.State          (MonadState, gets)
import qualified Data.Text                    as T
import qualified GI.Gtk                       as Gtk
import           Luukasa.AppState             (AppState)
import qualified Luukasa.AppState             as App
import           Luukasa.Event.Handler.Common (gtkEntryToUiState)
import qualified Luukasa.Generate             as Gen
import qualified Luukasa.UiState              as Ui

{-
TODO:

There's bit of repetition going on. See what kind of fuckery could solve it.
The scope of this modal code isn't too massive, so another option is to just
leave it remove this comment.

-}
entryValueUpdated
    :: (MonadIO m, MonadState AppState m)
    => Gtk.Entry
    -> (T.Text -> Maybe a)
    -> (a -> Ui.FrameGenModal -> Ui.FrameGenModal)
    -> m ()
entryValueUpdated inputWidget parseValue updateModel = do
    uiState <- gets App.uiState
    uiState' <- gtkEntryToUiState inputWidget parseValue
        (\x ui -> ui { Ui.frameGenModal = updateModel x (Ui.frameGenModal ui) }) uiState
    App.putUiState uiState'

setSeqType :: MonadState AppState m => Gen.SeqType -> m ()
setSeqType seqType = do
    ui <- gets App.uiState
    let seqGen = Ui.frameGenModal ui
    App.putUiState ui { Ui.frameGenModal = seqGen { Ui.seqType = seqType } }

setAccelType :: MonadState AppState m => Gen.AccelType -> m ()
setAccelType accelType = do
    ui <- gets App.uiState
    let seqGen = Ui.frameGenModal ui
    App.putUiState ui { Ui.frameGenModal = seqGen { Ui.accelType = accelType } }

setTargetType :: MonadState AppState m => Gen.TargetType -> m ()
setTargetType targetType = do
    ui <- gets App.uiState
    let seqGen = Ui.frameGenModal ui
    App.putUiState ui { Ui.frameGenModal = seqGen { Ui.targetType = targetType } }
