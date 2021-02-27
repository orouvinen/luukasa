{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Units (Radians, Degrees, mkRadians, mkDegrees, deg, getDegrees, rad, getRadians) where

newtype Radians = Radians { getRadians :: Double } deriving newtype (Show, Num)
newtype Degrees = Degrees { getDegrees :: Double } deriving newtype (Show, Num)

-- TODO: the "smart" constructors aren't so

mkDegrees :: Double -> Degrees
mkDegrees deg = Degrees deg

mkRadians :: Double -> Radians
mkRadians rad = Radians rad

deg :: Radians -> Degrees
deg (Radians r) = Degrees $ r * 180 / pi

rad :: Degrees -> Radians
rad (Degrees d) = Radians $ d * pi / 180