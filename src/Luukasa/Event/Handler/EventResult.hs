{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Luukasa.Event.Handler.EventResult where

import           Control.Monad.State   (MonadState)
import           Luukasa.AnimatorState (AnimatorState)
import           Luukasa.AppState      (AppState (..), putAnimatorState)
import           Luukasa.Common        (ErrorMessage)
import           Luukasa.EditorAction  (ActionResult)

type EventResult a = Either ErrorMessage a

toEventResult :: Monad m => ActionResult -> a -> m (EventResult a)
toEventResult res retval =
    return $ case res of
        Right _  -> Right retval
        Left err -> Left err

updateAnimatorState :: MonadState AppState m => Either a AnimatorState -> m ()
updateAnimatorState = \case
    Left _         -> return ()
    Right newState -> putAnimatorState newState
