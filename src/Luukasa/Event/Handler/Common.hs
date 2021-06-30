module Luukasa.Event.Handler.Common where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text              as T
import qualified GI.Gtk                 as Gtk
import           Luukasa.UiState        (UiState)


{-|
    gtkEntryToUiState tries to read a value from GTK entry and if successful, stores in UiState using
    specified setter function.
-}
gtkEntryToUiState
    :: (MonadIO m)
    => Gtk.Entry                -- ^ The Gtk.Entry widget to read value from
    -> (T.Text -> Maybe a)      -- ^ Function to parse the value from Text to desired type.
    -> (a -> UiState -> UiState) -- ^ Setter that receives the parsed value and current UiState. Should return updated UiState.
    -> UiState                  -- ^ Initial UiState
    -> m UiState
gtkEntryToUiState entry parseValue updateUiState originalUiState = do
    input <- Gtk.entryGetText entry
    return $
        case parseValue input of
            Nothing  -> originalUiState
            Just val -> updateUiState val originalUiState

gtkRadioToUiState :: MonadIO m => (UiState -> UiState) -> UiState -> m UiState
gtkRadioToUiState updateUiState originalUiState =
    return $ updateUiState originalUiState

