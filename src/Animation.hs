{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
    To the users of the module, frame numbers start from 1.
    Internally, _currentFrame is zero based so conversions
    happen at currentFrame & setCurrentFrame.
-}
module Animation
    ( Animation
    , FrameNum
    , mkFrameNum
    , currentFrame
    , setCurrentFrame
    , currentTimeCode
    , mkAnimation
    , appendFrame
    , deleteFrame
    , deleteCurrentFrame
    , currentFrameBody
    , setCurrentFrameBody)
    where
import           Body          (Body)
import           Data.Sequence (Seq, (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Frame         (Frame)
import qualified Frame         as F

data Animation = Animation
    { _frames       :: Seq Frame
    , _currentFrame :: Int
    , _fps          :: Int
    }

newtype FrameNum = FrameNum Int deriving (Num, Eq, Ord)

mkFrameNum :: Int -> FrameNum
mkFrameNum n = FrameNum $ if n < 1 then 1 else n

data TimeCode = TimeCode
    { hour   :: Int
    , minute :: Int
    , second :: Int
    , frame  :: Int
    }

instance Show Animation where
    show a = "FPS: " ++ show (_fps a) ++ " frm:" ++ show (_currentFrame a)
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

currentFrame :: Animation -> FrameNum
currentFrame = FrameNum . (+ 1) . _currentFrame

setCurrentFrame :: Animation -> FrameNum -> Animation
setCurrentFrame animation (FrameNum n)
    | n > numFrames = animation { _currentFrame = 0 }
    | n < 1         = animation { _currentFrame = numFrames - 1 }
    | otherwise     = animation { _currentFrame = n - 1 }
      where
        numFrames = length (_frames animation)


currentTimeCode :: Animation -> String
currentTimeCode animation =
    show $ frameTimeCode (_fps animation) (_currentFrame animation)

frameTimeCode :: Int -> Int -> TimeCode
frameTimeCode fps num =
    let totalSeconds = num `div` fps
        totalMinutes = totalSeconds `div` 60

        hours' = totalMinutes `div` 60
        minutes' = totalMinutes `mod` 60
        seconds' = totalSeconds `mod` 60
        frame' = (num `mod` fps) + 1
    in TimeCode hours' minutes' seconds' frame'

-- | The current body that's the object of edits & rendering
currentFrameBody :: Animation -> Body
currentFrameBody animation =
    let frame = Seq.index (_frames animation) (_currentFrame animation)
    in F.body frame

-- | Replace the active frame's body
setCurrentFrameBody :: Animation -> Body -> Animation
setCurrentFrameBody animation body =
    let frame = Seq.index (_frames animation) (_currentFrame animation)
        newFrame = F.setBody frame body
    in setFrame animation newFrame (currentFrame animation)

appendFrame :: Animation -> Body -> Animation
appendFrame animation body =
    let numFrames = length (_frames animation)
        newFrame = F.mkFrame (_fps animation) (numFrames + 1) body
    in appendFrame' animation newFrame

deleteCurrentFrame :: Animation -> Animation
deleteCurrentFrame a =
    if length (_frames a) == 1
        then a
        else deleteFrame a (currentFrame a)

deleteFrame :: Animation -> FrameNum -> Animation
deleteFrame a (FrameNum i) = a { _frames = frames }
  where
    frameIndex = i - 1
    frames = Seq.filter (\f -> F.num f /= frameIndex) (_frames a)

setFrame :: Animation -> Frame -> FrameNum -> Animation
setFrame a f (FrameNum i) = a { _frames = frames }
  where
    frames = Seq.adjust (const f) (i - 1) (_frames a)

appendFrame' :: Animation -> Frame -> Animation
appendFrame' a f = a { _frames = frames }
  where
    frames = _frames a |> f

prependFrame' :: Animation -> Frame -> Animation
prependFrame' a f = a { _frames = frames }
  where
    frames = f <| _frames a

-- insertFrame' :: Animation -> Frame -> Int -> Animation
-- insertFrame' a f i =
--     let (start, end) = Seq.splitAt i (_frames a)
--         frames = start >< Seq.singleton f >< end
--     in a { _frames = frames }

