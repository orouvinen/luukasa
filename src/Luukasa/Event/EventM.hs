{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Luukasa.Event.EventM where

import           Luukasa.AppState           (AppState)
import           Luukasa.Event.Keyboard     (HasKeyEvent (..))
import           Luukasa.Event.Mouse        (HasMouseEvent (..))
import           Luukasa.Event.Ui.UiElement (HasTreeView (..),
                                             HasUiListStore (..))

import           Control.Monad              ((>=>))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (MonadReader, ReaderT, ask,
                                             runReaderT)
import           Control.Monad.State        (MonadState, get, put)
import           Data.IORef                 (IORef, readIORef, writeIORef)
import qualified GI.Gdk                     as Gdk
import qualified GI.Gtk                     as Gtk

newtype EventM a = EventM { runEventM :: ReaderT (IORef AppState) IO a }
    deriving (Functor, Applicative, Monad, MonadReader (IORef AppState), MonadIO)

instance MonadState AppState EventM where
    get = ask >>= liftIO . readIORef
    put s = ask >>= \stateRef -> liftIO $ writeIORef stateRef s

instance HasKeyEvent EventM where
    getKey = Gdk.getEventKeyKeyval >=> Gdk.keyvalToUpper

instance HasMouseEvent EventM where
    getScrollDirection = Gdk.getEventScrollDirection
    clickPos e = (,) <$> Gdk.getEventButtonX e <*> Gdk.getEventButtonY e
    motionPos e = (,) <$> Gdk.getEventMotionX e <*> Gdk.getEventMotionY e
    clickModifiers = Gdk.getEventButtonState
    motionModifiers = Gdk.getEventMotionState

instance HasUiListStore EventM where
    clearListStore = Gtk.listStoreClear
    insertListRow listStore gvalues = Gtk.listStoreInsertWithValuesv
        listStore
        (-1)
        [0..fromIntegral $ length gvalues - 1]
        gvalues
    listStoreSetValue = Gtk.listStoreSetValue

instance HasTreeView EventM where
    getIterAsString = Gtk.treeModelGetStringFromIter
    getIterFromString listStore s = do
        iterRes <- Gtk.treeModelGetIterFromString listStore s
        return $
            if fst iterRes
                then Just $ snd iterRes
                else Nothing

runEvent :: IORef AppState -> EventM a -> IO a
runEvent stateRef handler = runReaderT (runEventM handler) stateRef
