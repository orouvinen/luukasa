{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}

module Luukasa.Generate where

import           Luukasa.Joint (JointId)
import           Units         (Degrees)

data SeqType = Still | Translate | Rotate

{- Note:
If needed, ad-hoc single translations made by hand can still be described
with e.g. Rotate 20 degrees during one single frame:

GenSequence
    { seqType       = Rotate
    , seqDuration   = SeqDuration (FrameSpan 1 1) (RotateDelta 20)
    , seqObject     = [1]
    ,
    }

 -- TODO: the above comment still stands, but the data model has changed a bit
-}


data TargetType = TargetPos | TargetLocalRot | TargetWorldRot | TargetRotateDelta
data SeqTarget (t :: TargetType) where
    ToPosition :: Double -> Double -> SeqTarget 'TargetPos
    ToLocalRot :: Degrees -> SeqTarget 'TargetLocalRot
    ToWorldRot :: Degrees -> SeqTarget 'TargetWorldRot
    RotateDelta :: Degrees -> SeqTarget 'TargetRotateDelta

data FrameSpan = FrameSpan Int Int

data SeqDuration (t :: TargetType) = SeqDuration FrameSpan (SeqTarget t)

newtype SeqObject = SeqObject [JointId]

data AccelType = AccelPerFrame | AccelPerSecond
data SeqAccel (t :: AccelType) where
    PerFrame :: Double -> SeqAccel 'AccelPerFrame
    PerSecond :: Double -> SeqAccel 'AccelPerSecond


data GenSequence = GenSequence
    { seqType     :: SeqType
    , seqDuration :: DurationWrapper
    , seqObject   :: SeqObject
    , seqAcc      :: AccelWrapper
    }

data DurationWrapper where
    WrapDuration :: SeqDuration (t :: TargetType) -> DurationWrapper

data AccelWrapper where
    WrapAccel :: SeqAccel (a :: AccelType) -> AccelWrapper
