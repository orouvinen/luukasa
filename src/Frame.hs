{-# LANGUAGE OverloadedStrings #-}

module Frame where
import           Data.Text (Text)
import qualified Data.Text as T

import           Body
data Frame = Frame
    { num  :: Int
    , body :: Body
    }

mkFrame :: Int -> Int -> Body -> Frame
mkFrame fps num body = Frame { num = num, body = body }
