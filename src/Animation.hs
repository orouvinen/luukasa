{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Animation
    ( Animation
    , FrameNum
    , mkFrameNum
    , currentFrameNum
    , setCurrentFrame
    , frameStep
    , currentTimeCode
    , mkAnimation
    , appendFrame
    , deleteFrame
    , deleteCurrentFrame
    , currentFrameBody
    , setCurrentFrameBody)
    where

import           Body          (Body)
import           Data.Foldable (toList)
import           Data.Sequence (Seq, (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Frame         (Frame)
import qualified Frame         as F

data Animation = Animation
    { _frames       :: Seq Frame
    , _currentFrame :: Int
    , _fps          :: Int
    }

newtype FrameNum = FrameNum { unFrameNum :: Int }

instance Show FrameNum where
    show (FrameNum x) = show x

mkFrameNum :: Int -> FrameNum
mkFrameNum n = FrameNum $ if n < 0 then 0 else n

data TimeCode = TimeCode
    { hour   :: Int
    , minute :: Int
    , second :: Int
    , frame  :: Int
    }

instance Show Animation where
    show a = "FPS: "
        ++ show (_fps a)
        ++ " frm: "
        ++ show (_currentFrame a) ++ "\n"
        ++ unlines (show <$> toList (_frames a))


instance Show TimeCode where
    show tc = timeDigit (hour tc) ++ ":"
            ++ timeDigit (minute tc) ++ ":"
            ++ timeDigit (second tc) ++ ":"
            ++ frameDigit (frame tc)
      where
        timeDigit x
            | x < 10    = "0" ++ show x
            | otherwise = show x
        frameDigit x
            | x < 10    = "00" ++ show x
            | otherwise = "0" ++ show x

mkAnimation :: Int -> Animation
mkAnimation fps = Animation
    { _frames = Seq.Empty
    , _currentFrame = 0
    , _fps = fps
    }

currentFrameNum :: Animation -> FrameNum
currentFrameNum = FrameNum . _currentFrame

setCurrentFrame :: Animation -> FrameNum -> Animation
setCurrentFrame animation (FrameNum n)
    | n > numFrames - 1 = animation { _currentFrame = numFrames - 1 }
    | n < 0             = animation { _currentFrame = 0 }
    | otherwise         = animation { _currentFrame = n }
      where
        numFrames = Seq.length (_frames animation)

frameStep :: Animation -> Int -> Animation
frameStep a n =
    let numFrames = Seq.length (_frames a)
        frameNum = (_currentFrame a + n) `mod` numFrames
    in a { _currentFrame = frameNum }

currentTimeCode :: Animation -> String
currentTimeCode animation =
    show $ frameTimeCode (_fps animation) (currentFrameNum animation)

frameTimeCode :: Int -> FrameNum -> TimeCode
frameTimeCode fps (FrameNum num) =
    let totalSeconds = num `div` fps
        totalMinutes = totalSeconds `div` 60

        hours' = totalMinutes `div` 60
        minutes' = totalMinutes `mod` 60
        seconds' = totalSeconds `mod` 60
        frame' = (num `mod` fps) + 1
    in TimeCode hours' minutes' seconds' frame'

-- | The current body that's the object of edits & rendering
currentFrameBody :: Animation -> Body
currentFrameBody a =
    let frame = Seq.index (_frames a) (_currentFrame a)
    in F.body frame

-- | Replace the active frame's body
setCurrentFrameBody :: Animation -> Body -> Animation
setCurrentFrameBody animation body =
    let frame = Seq.index (_frames animation) (_currentFrame animation)
        newFrame = F.setBody frame body
    in setFrameData animation newFrame (currentFrameNum animation)

appendFrame :: Animation -> Body -> Animation
appendFrame animation body =
    let numFrames = Seq.length (_frames animation)
        frameNum = numFrames
        newFrame = F.mkFrame (_fps animation) frameNum body
    in appendFrame' animation newFrame

deleteCurrentFrame :: Animation -> Animation
deleteCurrentFrame a =
    if Seq.length (_frames a) == 1
        then a
        else deleteFrame a (currentFrameNum a)

deleteFrame :: Animation -> FrameNum -> Animation
deleteFrame a (FrameNum n) = a { _frames = frames, _currentFrame = newCurrentFrame }
  where
    frames = Seq.filter (\f -> F.num f /= n) (_frames a)
    numFrames = Seq.length frames
    newCurrentFrame = min (numFrames - 1) n

setFrameData :: Animation -> Frame -> FrameNum -> Animation
setFrameData a f (FrameNum i) = a { _frames = frames }
  where
    frames = Seq.adjust (const f) i (_frames a)

appendFrame' :: Animation -> Frame -> Animation
appendFrame' a f = a { _frames = frames }
  where
    frames = _frames a |> f

prependFrame' :: Animation -> Frame -> Animation
prependFrame' a f = a { _frames = frames }
  where
    frames = f <| _frames a

fixFrameNums :: Animation -> Animation
fixFrameNums = undefined -- TODO: flatten frame sequence numbers

-- insertFrame' :: Animation -> Frame -> Int -> Animation
-- insertFrame' a f i =
--     let (start, end) = Seq.splitAt i (_frames a)
--         frames = start >< Seq.singleton f >< end
--     in a { _frames = frames }

