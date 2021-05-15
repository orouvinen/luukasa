module Luukasa.Event.Ui.UiElement where

import           Data.Int  (Int32)
import           Data.Text (Text)
import qualified GI.Gtk    as Gtk

class (Monad m, HasTreeView m) => HasUiListStore m where
    clearListStore :: Gtk.ListStore -> m ()
    insertListRow :: Gtk.ListStore -> [Gtk.GValue] -> m Gtk.TreeIter
    listStoreSetValue :: Gtk.ListStore -> Gtk.TreeIter -> Int32 -> Gtk.GValue -> m ()

class Monad m => HasTreeView m where
    getIterAsString :: Gtk.ListStore -> Gtk.TreeIter -> m Text
    getIterFromString :: Gtk.ListStore -> Text -> m (Maybe Gtk.TreeIter)

