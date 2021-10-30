{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Luukasa.Event.Handler.FileMenu where

import           Control.Monad            (forM_, void)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.State      (MonadState, gets)
import           Data.Functor             ((<&>))
import           Data.Text                (Text, pack, unpack)
import           GI.Gtk                   (AttrOp ((:=)), new)
import qualified GI.Gtk                   as Gtk
import           Luukasa.AnimatorState    (AnimatorState)
import qualified Luukasa.AnimatorState    as ST
import           Luukasa.AppState         (AppState)
import qualified Luukasa.AppState         as App
import           Luukasa.Common           (ErrorMessage)
import           Luukasa.Event.JsonFileIO (JsonFileIO (..))

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
