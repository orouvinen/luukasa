{-# LANGUAGE FlexibleContexts #-}
module Luukasa.AppState where

import           Luukasa.AnimatorState (AnimatorState, initialAnimatorState)
import           Luukasa.UiState       (UiState, initialUiState)
import Control.Monad.State (modify, MonadState)

data AppState = AppState
    { animatorState :: AnimatorState
    , uiState       :: UiState
    }

initialAppState :: AppState
initialAppState = AppState
    { animatorState = initialAnimatorState
    , uiState = initialUiState
    }


putUiState :: MonadState AppState m => UiState -> m ()
putUiState x = modify (\s -> s { uiState = x })

putAnimatorState :: MonadState AppState m => AnimatorState -> m ()
putAnimatorState x = modify (\s -> s { animatorState = x })
