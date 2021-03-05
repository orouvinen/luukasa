{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE DerivingStrategies #-}

module Units (Radians, Degrees, mkRadians, mkDegrees, deg, getDegrees, rad, getRadians) where

newtype Radians = Radians { getRadians :: Double } deriving (Show, Num, Ord, Eq)
newtype Degrees = Degrees { getDegrees :: Double } deriving (Show, Num, Ord, Eq)

mkDegrees :: Double -> Degrees
mkDegrees deg = Degrees $ fixDegrees deg
    where fixDegrees x
            | x >= 360  = x - 360
            | x < 0     = x + 360
            | otherwise = x

mkRadians :: Double -> Radians
mkRadians rad = Radians $ fixRadians rad
    where fixRadians x
            | x >= pi * 2   = x - (pi * 2)
            | x < 0         = x + (pi * 2)
            | otherwise     = x

deg :: Radians -> Degrees
deg (Radians r) = Degrees $ r * 180 / pi

rad :: Degrees -> Radians
rad (Degrees d) = Radians $ d * pi / 180
