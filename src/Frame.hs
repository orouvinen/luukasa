{-# LANGUAGE OverloadedStrings #-}

module Frame where
import           Data.Text (Text)
import qualified Data.Text as T

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

timeDigit :: Int -> String
timeDigit x
    | x < 10    = "0" ++ show x
    | otherwise = show x

frameDigit :: Int -> String
frameDigit x
    | x < 10    = "00" ++ show x
    | otherwise = "0" ++ show x

instance Show TimeCode where
    show tc = timeDigit (hour tc)
            ++ ":"
            ++ timeDigit (minute tc)
            ++ ":"
            ++ timeDigit (second tc)
            ++ ":"
            ++ frameDigit (frame tc)

mkFrame :: Int -> Int -> Body -> Frame
mkFrame fps num body = Frame { num = num, body = body, timeCode = frameTimeCode num fps }

frameTimeCode :: Int -> Int -> TimeCode
frameTimeCode fps num = TimeCode 0 0 0 0 -- TODO
