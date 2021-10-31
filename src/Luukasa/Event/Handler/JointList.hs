{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Luukasa.Event.Handler.JointList where

import           Control.Monad              (foldM)
import           Control.Monad.State        (MonadState, gets)
import           Data.Int                   (Int32)
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified GI.Gtk                     as Gtk
import qualified Luukasa.Animation          as A
import qualified Luukasa.AnimatorState      as ST
import           Luukasa.AppState           (AppState)
import qualified Luukasa.AppState           as App
import qualified Luukasa.Body               as B
import           Luukasa.Event.Ui.UiElement
import           Luukasa.Joint              (JointId)
import qualified Luukasa.Joint              as J

{- | updateJointList will update the data shown on the joint list ListStore to
match the joint data in the app's AnimatorState.
-}
updateJointList
    :: (MonadState AppState m, UiListStore m)
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


{- | Wired up to onCellRendererTextEdited signal,
setJointListCellValue updates a value in a cell in the joint ListStore.

Note that this only updates the value stored in the list store -
the value is stored in the app's AnimatorState as well and must be updated
there separately.

For the whole picture, see the construction of the handler to the aforementioned signal.
-}
setJointListCellValue
    :: (MonadState AppState m, UiListStore m)
    => Gtk.ListStore -- ^ list store to update
    -> Text          -- ^ TreeView path as given by Gdk
    -> Gtk.GValue    -- ^ Value to set to desired cell
    -> Int32         -- ^ Column number for the data
    -> m (Maybe JointId)
setJointListCellValue jointListStore path cellVal colNum = do
    mbIter <- getIterFromString jointListStore path
    case mbIter of
        Nothing -> return Nothing
        Just iter -> do
            listStoreSetValue jointListStore iter colNum cellVal

            appState <- gets App.animatorState
            let jointIterLookup = ST.jointIterLookup appState
                jointId = jointIterLookup ! path
            return $ Just jointId
