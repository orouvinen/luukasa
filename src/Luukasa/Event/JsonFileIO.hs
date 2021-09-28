{-# LANGUAGE MultiParamTypeClasses #-}

module Luukasa.Event.JsonFileIO where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON, ToJSON)

class (MonadIO m, ToJSON a, FromJSON a) => JsonFileIO a m where
    writeJson :: FilePath -> a -> m ()
    readJson :: FilePath -> m (Either String a)
