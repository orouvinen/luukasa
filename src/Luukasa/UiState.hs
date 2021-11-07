{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
-- UI state that doesn't need to be persisted when saving to file.

module Luukasa.UiState where

import qualified Luukasa.Generate as Gen
import qualified Units

data UiState = UiState
    { seqGenModal      :: SeqGenModal
    , isCellEditActive :: Bool
    }

data SeqGenModal = SeqGenModal
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
    { seqGenModal = SeqGenModal
        { targetType = Gen.TargetPos
        , targetX = 0
        , targetY = 0
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

seqGenModalToSequenceDescriptor :: SeqGenModal -> Gen.GenSequence
seqGenModalToSequenceDescriptor s =
        let frameSpan   = Gen.FrameSpan (startFrame s) (endFrame s)
        in
            Gen.GenSequence
                { Gen.seqTarget =
                    case targetType s of
                        Gen.TargetPos         -> Gen.WrapTarget $ Gen.ToPosition (targetX s) (targetY s)
                        Gen.TargetLocalRot    -> Gen.WrapTarget $ Gen.ToLocalRot (Units.mkDegrees $ targetLocalRot s)
                        Gen.TargetWorldRot    -> Gen.WrapTarget $ Gen.ToWorldRot (Units.mkDegrees $ targetWorldRot s)
                        Gen.TargetRotateDelta -> Gen.WrapTarget $ Gen.RotateDelta (Units.mkDegrees $ targetRotDelta s)
                , Gen.seqFrameSpan = frameSpan
                , Gen.seqObject = Gen.SeqObject (-1) -- TODO
                , Gen.seqAcc =
                    case accelType s of
                        Gen.AccelPerFrame -> Gen.WrapAccel $ Gen.PerFrame (accel s)
                        Gen.AccelPerSecond -> Gen.WrapAccel $ Gen.PerSecond (accel s)
                }
