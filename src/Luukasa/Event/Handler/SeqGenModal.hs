{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Luukasa.Event.Handler.SeqGenModal
    ( entryValueUpdated
    , setAccelType
    , setTargetType
    , Gen.TargetType (..)
    , Gen.AccelType (..)
    ) where

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.State          (MonadState, gets)
import qualified Data.Text                    as T
import qualified GI.Gtk                       as Gtk
import           Luukasa.AppState             (AppState)
import qualified Luukasa.AppState             as App
import           Luukasa.Event.Handler.Common (gtkEntryToUiState)
import qualified Luukasa.Generate             as Gen
import qualified Luukasa.UiState              as Ui

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
