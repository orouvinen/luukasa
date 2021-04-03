module Frame where

import           Body

data TimeCode = TimeCode
    { hr  :: Int
    , min :: Int
    , sec :: Int
    , frm :: Int
    } deriving Show

data Frame = Frame
    { num      :: Int
    , timeCode :: TimeCode
    , body     :: Body
    }

mkFrame :: Int -> Int -> Body -> Frame
mkFrame fps num body = Frame { num = num, body = body, timeCode = frameTimeCode num fps }

frameTimeCode :: Int -> Int -> TimeCode
frameTimeCode fps num = TimeCode 0 0 0 0 -- TODO
