{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Units
    ( Radians
    , Degrees
    , mkRadians
    , mkDegrees
    , deg
    , getDegrees
    , rad
    , getRadians
    ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Fixed (mod')

newtype Radians = Radians { getRadians :: Double } deriving (Show, Num, Ord, Eq)
newtype Degrees = Degrees { getDegrees :: Double } deriving (Show, Num, Ord, Eq, FromJSON, ToJSON)

mkDegrees :: Double -> Degrees
mkDegrees d = Degrees $ mod' d 360

mkRadians :: Double -> Radians
mkRadians r = Radians $ mod' r (pi * 2)

deg :: Radians -> Degrees
deg (Radians r) = Degrees $ r * 180 / pi

rad :: Degrees -> Radians
rad (Degrees d) = Radians $ d * pi / 180
