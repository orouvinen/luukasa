module Frame where

import           Body
data TimeCode = TimeCode
    { hour   :: Int
    , minute :: Int
    , second :: Int
    , frame  :: Int
    }
data Frame = Frame
    { num      :: Int
    , timeCode :: TimeCode
    , body     :: Body
    }

-- TODO: zero pad components
instance Show TimeCode where
    show tc
        = show (hour tc)
        ++ ":"
        ++ show (minute tc)
        ++ ":"
        ++ show (second tc)
        ++ ":"
        ++ show (frame tc)

mkFrame :: Int -> Int -> Body -> Frame
mkFrame fps num body = Frame { num = num, body = body, timeCode = frameTimeCode num fps }

frameTimeCode :: Int -> Int -> TimeCode
frameTimeCode fps num = TimeCode 0 0 0 0 -- TODO
