{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Luukasa.Event.Handler.Playback where

import           Control.Monad                     (when)
import           Control.Monad.State               (MonadState, gets)
import           Data.Maybe                        (fromJust)
import qualified Luukasa.Animation                 as Animation
import           Luukasa.AnimatorState             (ActionState (..))
import qualified Luukasa.AnimatorState             as ST
import           Luukasa.AppState                  (AppState)
import qualified Luukasa.AppState                  as App
import           Luukasa.Common
import           Luukasa.EditorAction              (dispatchAction)
import qualified Luukasa.EditorAction              as E
import           Luukasa.Event.Handler.EventResult (updateAnimatorState)

startPlayback :: MonadState AppState m => TimerCallbackId -> TimestampUs -> m ()
startPlayback timerCallbackId timestamp = do
    s <- gets App.animatorState
    App.putAnimatorState s
        { ST.actionState = AnimationPlayback timerCallbackId
        , ST.frameStart = Just timestamp
        }

stopPlayback :: MonadState AppState m => m ()
stopPlayback = do
    s <- gets App.animatorState
    App.putAnimatorState s { ST.actionState = Idle, ST.frameStart = Nothing }

playbackTick :: MonadState AppState m => TimestampUs -> m Bool
playbackTick timestamp = do
    s <- gets App.animatorState
    let frameIntervalUs = (1.0 / (fromIntegral (Animation.fps $ ST.animation s) :: Double)) * 1000 * 1000
        lastFrame = fromJust (ST.frameStart s)
        sinceLastFrame = timestamp - lastFrame
        renderNeeded = fromIntegral sinceLastFrame >= frameIntervalUs

    when renderNeeded $ do
        -- timestamp might not be totally accurate for frameStart if
        -- this frame is delayed. Should probably set the originally intended frameStart?
        let result = dispatchAction s { ST.frameStart = Just timestamp } (E.FrameStep 1)
        updateAnimatorState result

    return renderNeeded
