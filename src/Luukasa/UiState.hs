{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
-- UI state that doesn't need to be persisted when saving to file.

module Luukasa.UiState where

import qualified Luukasa.Generate as Gen
import qualified Units

data UiState = UiState
    { frameGenModal    :: FrameGenModal
    , isCellEditActive :: Bool
    }

-- This should live in src/Luukasa/Event/Handler/SeqGenModal.hs ?
data FrameGenModal = FrameGenModal
    { targetType     :: Gen.TargetType
    , targetX        :: Double
    , targetY        :: Double
    , targetLocalRot :: Double
    , targetWorldRot :: Double
    , targetRotDelta :: Double
    , startFrame     :: Int
    , endFrame       :: Int
    , accel          :: Double
    , accelType      :: Gen.AccelType
    }

initialUiState :: UiState
initialUiState = UiState
    { frameGenModal = FrameGenModal
        { targetType = Gen.TargetPos
        , targetX = 0
        , targetY = 0
        -- TODO: only one degrees input field needed; the semantics of the value is determined by targetType.
        -- Remove extra inputs from UI and here.
        , targetLocalRot = 0
        , targetWorldRot = 0
        , targetRotDelta = 0
        , startFrame = 0
        , endFrame = 0
        , accel = 0
        , accelType = Gen.AccelPerFrame
        }
    , isCellEditActive = False
    }

mapGenModalToSeqDescriptor :: FrameGenModal-> Gen.GenSequence
mapGenModalToSeqDescriptor s =
        let frameSpan   = Gen.FrameSpan (startFrame s) (endFrame s)
        in
            Gen.GenSequence
                { Gen.seq =
                    case targetType s of
                        Gen.TargetPos         -> Gen.WrapSequence $ Gen.Sequence frameSpan (Gen.ToPosition (targetX s) (targetY s))
                        Gen.TargetLocalRot    -> Gen.WrapSequence $ Gen.Sequence frameSpan (Gen.ToLocalRot (Units.mkDegrees $ targetLocalRot s))
                        Gen.TargetWorldRot    -> Gen.WrapSequence $ Gen.Sequence frameSpan (Gen.ToWorldRot (Units.mkDegrees $ targetWorldRot s))
                        Gen.TargetRotateDelta -> Gen.WrapSequence $ Gen.Sequence frameSpan (Gen.RotateDelta (Units.mkDegrees $ targetRotDelta s))
                , Gen.seqObject = Gen.SeqObject [] -- TODO
                , Gen.seqAcc =
                    case accelType s of
                        Gen.AccelPerFrame -> Gen.WrapAccel $ Gen.PerFrame (accel s)
                        Gen.AccelPerSecond -> Gen.WrapAccel $ Gen.PerSecond (accel s)
                }
