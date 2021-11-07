{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}

module Luukasa.Generate where

import           Luukasa.Joint (JointId)
import           Units         (Degrees)

data TargetType = TargetPos | TargetLocalRot | TargetWorldRot | TargetRotateDelta
data SeqTarget (t :: TargetType) where
    ToPosition :: Double -> Double -> SeqTarget 'TargetPos
    ToLocalRot :: Degrees -> SeqTarget 'TargetLocalRot
    ToWorldRot :: Degrees -> SeqTarget 'TargetWorldRot
    RotateDelta :: Degrees -> SeqTarget 'TargetRotateDelta

data FrameSpan = FrameSpan Int Int

data Sequence (t :: TargetType) = Sequence FrameSpan (SeqTarget t)

newtype SeqObject = SeqObject JointId

data AccelType = AccelPerFrame | AccelPerSecond
data SeqAccel (t :: AccelType) where
    PerFrame :: Double -> SeqAccel 'AccelPerFrame
    PerSecond :: Double -> SeqAccel 'AccelPerSecond

data GenSequence = GenSequence
    { seqTarget    :: TargetWrapper
    , seqFrameSpan :: FrameSpan
    , seqObject    :: SeqObject
    , seqAcc       :: AccelWrapper
    }

data TargetWrapper where
    WrapTarget :: SeqTarget (t :: TargetType) -> TargetWrapper

data AccelWrapper where
    WrapAccel :: SeqAccel (a :: AccelType) -> AccelWrapper
