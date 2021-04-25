{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Luukasa.Animation
    ( Animation
    , FrameNum
    , fps
    , mkFrameNum
    , mkAnimation
    , currentFrameNum
    , currentTimeCode
    , frameStep
    , setCurrentFrame
    , frameData
    , setFrameData
    , currentFrameData
    , setCurrentFrameData
    , appendFrame
    , prependFrame
    , deleteFrame
    , deleteCurrentFrame
    ) where

import           Data.Aeson
import           Data.Foldable (toList)
import           Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import           GHC.Generics
data Animation a = Animation
    { _frames       :: Seq a
    , _currentFrame :: Int
    , _fps          :: Int
    } deriving (Generic, Functor)

instance ToJSON a => ToJSON (Animation a)
instance FromJSON a => FromJSON (Animation a)

newtype FrameNum = FrameNum Int

instance Show FrameNum where
    show (FrameNum x) = show x

mkFrameNum :: Int -> FrameNum
mkFrameNum n = FrameNum $ if n < 0 then 0 else n

data TimeCode = TimeCode
    { _hr  :: Int
    , _min :: Int
    , _sec :: Int
    , _frm :: Int
    }

instance Show a => Show (Animation a) where
    show a = "FPS: "
        ++ show (_fps a)
        ++ " frm: "
        ++ show (_currentFrame a) ++ "\n"
        ++ unlines (show <$> toList (_frames a))

instance Show TimeCode where
    show tc = timeDigit (_hr tc) ++ ":"
            ++ timeDigit (_min tc) ++ ":"
            ++ timeDigit (_sec tc) ++ ":"
            ++ frameDigit (_frm tc)
      where
        timeDigit x
            | x < 10    = "0" ++ show x
            | otherwise = show x
        frameDigit x
            | x < 10    = "00" ++ show x
            | otherwise = "0" ++ show x

mkAnimation :: Int -> Animation a
mkAnimation fps = Animation
    { _frames = Seq.Empty
    , _currentFrame = 0
    , _fps = fps
    }

fps :: Animation a -> Int
fps = _fps

currentFrameNum :: Animation a -> FrameNum
currentFrameNum = FrameNum . _currentFrame

frameTimeCode :: Int -> FrameNum -> TimeCode
frameTimeCode fps (FrameNum num) =
    let totalSeconds = num `div` fps
        totalMinutes = totalSeconds `div` 60

        hours' = totalMinutes `div` 60
        minutes' = totalMinutes `mod` 60
        seconds' = totalSeconds `mod` 60
        frame' = (num `mod` fps) + 1
    in TimeCode hours' minutes' seconds' frame'

currentTimeCode :: Animation a -> String
currentTimeCode animation =
    show $ frameTimeCode (_fps animation) (currentFrameNum animation)

frameStep :: Animation a -> Int -> Animation a
frameStep a n =
    let numFrames = Seq.length (_frames a)
        frameNum = (_currentFrame a + n) `mod` numFrames
    in a { _currentFrame = frameNum }

setCurrentFrame :: Animation a -> FrameNum -> Animation a
setCurrentFrame animation (FrameNum n)
    | n > numFrames - 1 = animation { _currentFrame = numFrames - 1 }
    | n < 0             = animation { _currentFrame = 0 }
    | otherwise         = animation { _currentFrame = n }
      where
        numFrames = Seq.length (_frames animation)

-- | Get data for given frame
frameData :: Animation a -> FrameNum -> a
frameData a (FrameNum i) = Seq.index (_frames a) i

-- | Set data for given frame
setFrameData :: Animation a -> a -> FrameNum -> Animation a
setFrameData a f (FrameNum i) = a { _frames = frames }
  where
    frames = Seq.update i f (_frames a)

-- | Replace the active frame's data
setCurrentFrameData :: Animation a -> a -> Animation a
setCurrentFrameData animation body = setFrameData animation body (currentFrameNum animation)

-- | Get data of the currently active frame
currentFrameData :: Animation a -> a
currentFrameData a = Seq.index (_frames a) (_currentFrame a)

-- | Insert new frame with data at the end of the animation sequence
appendFrame :: Animation a -> a -> Animation a
appendFrame a b = a { _frames = frames }
  where
    frames = _frames a |> b

-- | Insert new frame with data at the start of the animation sequence
prependFrame :: Animation a -> a -> Animation a
prependFrame a f = a { _frames = frames }
  where
    frames = f <| _frames a

-- | Delete currently active frame
deleteCurrentFrame :: Animation a -> Animation a
deleteCurrentFrame a =
    if Seq.length (_frames a) == 1
        then a
        else deleteFrame a (currentFrameNum a)

-- | Delete given frame
deleteFrame :: Animation a -> FrameNum -> Animation a
deleteFrame a (FrameNum n) = a { _frames = frames, _currentFrame = newCurrentFrame }
  where
      (start, end) = Seq.splitAt n (_frames a)
      frames = start <> Seq.drop 1 end
      newCurrentFrame = min n (Seq.length frames - 1)

-- insertFrame' :: Animation -> Frame -> Int -> Animation
-- insertFrame' a f i =
--     let (start, end) = Seq.splitAt i (_frames a)
--         frames = start >< Seq.singleton f >< end
--     in a { _frames = frames }

