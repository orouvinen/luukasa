{-# LANGUAGE ViewPatterns #-}
module Calc (rotd, distance, angle, rotdWithRange) where

import           Data.Fixed   (mod')
import           LimitedRange (LimitedRange)
import qualified LimitedRange
import           Units        (Degrees, deg, getDegrees, mkDegrees,
                               mkDegreesUnlimited, mkRadians)

rotd :: Degrees -> Double -> Degrees
rotd (getDegrees -> x) delta = mkDegrees $ rotated $ x + delta
  where
    rotated degrees = mod' degrees 360

rotdWithRange :: Degrees -> Double -> LimitedRange Degrees -> Degrees
rotdWithRange degFrom@(getDegrees -> x) delta range =
    case LimitedRange.getEffectiveRange range of
        Just r -> LimitedRange.fitValue r attemptedRotation
          where
            attemptedRotation = mkDegreesUnlimited $ x + delta
        Nothing -> rotd degFrom delta

distance :: Double -> Double -> Double -> Double -> Double
distance x y x' y' =
    let dx = x - x'
        dy = y - y'
    in sqrt $ dx * dx + dy * dy

angle :: Double -> Double -> Double -> Double -> Degrees
angle parentX parentY childX childY =
    let dx = parentX - childX
        dy = parentY - childY
        -- Flip x & y so that 0 degrees is to the right, 90 down etc.
        -- also flip y coordinate for screen
        a = deg $ mkRadians $ atan2 dx (-dy)
    in rotd a 90
