module Animation (Animation, currentTimeCode, mkAnimation, appendFrame, currentFrameBody, setCurrentFrameBody) where

import           Body          (Body)
import           Data.Sequence (Seq, (<|), (><), (|>))
import qualified Data.Sequence as Seq
import           Frame         (Frame)
import qualified Frame         as F

data Animation = Animation
    { frames       :: Seq Frame
    , currentFrame :: Int
    , fps          :: Int
    }

-- A dummy instance at this point
instance Show Animation where
    show a = "FPS: " ++ show (fps a) ++ " frm:" ++ show (currentFrame a)

mkAnimation :: Int -> Animation
mkAnimation fps = Animation
    { frames = Seq.Empty
    , currentFrame = 0 -- Frame numbers are 1-based (0 is not a valid frame number)
    , fps = fps
    }

currentTimeCode :: Animation -> String
currentTimeCode animation =
    show $ F.timeCode $ Seq.index (frames animation) (currentFrame animation)

-- | The current body that's is the object of edits & rendering
currentFrameBody :: Animation -> Body
currentFrameBody animation =
    let frame = Seq.index (frames animation) (currentFrame animation)
    in F.body frame

-- | Replace the active frame's body
setCurrentFrameBody :: Animation -> Body -> Animation
setCurrentFrameBody animation body =
    let frame = Seq.index (frames animation) (currentFrame animation)
        newFrame = frame { F.body = body }
    in setFrame animation newFrame (currentFrame animation - 1)

appendFrame :: Animation -> Body -> Animation
appendFrame animation body =
    let numFrames = length (frames animation)
        newFrame = F.mkFrame (fps animation) (numFrames + 1) body
    in appendFrame' animation newFrame

setFrame :: Animation -> Frame -> Int -> Animation
setFrame a f i =
    let start = Seq.take (i - 1) (frames a)
        end = Seq.drop (i + 1) (frames a)
        frames' = start >< Seq.singleton f >< end
    in a { frames = frames' }

appendFrame' :: Animation -> Frame -> Animation
appendFrame' a f = a { frames = frames' }
  where
    frames' = frames a |> f

prependFrame' :: Animation -> Frame -> Animation
prependFrame' a f = a { frames = frames' }
  where
    frames' = f <| frames a

insertFrame' :: Animation -> Frame -> Int -> Animation
insertFrame' a f i =
    let (start, end) = Seq.splitAt i (frames a)
        frames' = start >< Seq.singleton f >< end
    in a { frames = frames' }

