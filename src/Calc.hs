{-# LANGUAGE ViewPatterns #-}
module Calc (rotd, distance, angle) where

import           Units (Degrees, deg, getDegrees, mkDegrees, mkRadians)

rotd :: Degrees -> Double -> Degrees
rotd (getDegrees -> x) delta = mkDegrees $ rotated $ x + delta
  where
      rotated degrees
        | degrees > 359 = degrees - 360
        | degrees < 0   = degrees + 360
        | otherwise     = degrees

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

