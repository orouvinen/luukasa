{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LimitedRange
    ( LimitedRange
    , mkRange
    , lower
    , upper
    , setLower
    , setUpper
    , getEffectiveRange
    , isRangeEffective
    , fitValue)
    where

import           Data.Aeson (FromJSON, ToJSON)
newtype LimitedRange a = Range (a, a) deriving (FromJSON, ToJSON, Show)

type Limitable a = (Ord a, Eq a, Show a)

mkRange :: Limitable a => a -> a -> LimitedRange a
mkRange x y = Range (lower', upper')
  where
    lower' = min x y
    upper' = max x y

setLower :: Limitable a => LimitedRange a -> a -> LimitedRange a
setLower r lower' = mkRange lower' upper'
  where
    upper' = upper r

setUpper :: Limitable a => LimitedRange a -> a -> LimitedRange a
setUpper r upper' = mkRange lower' upper'
  where
    lower' = lower r

lower :: Limitable a => LimitedRange a -> a
lower (Range r) = fst r

upper :: Limitable a => LimitedRange a -> a
upper (Range r) = snd r

isRangeEffective :: Limitable a => LimitedRange a -> Bool
isRangeEffective r = lower r /= upper r

getEffectiveRange :: Limitable a => LimitedRange a -> Maybe (LimitedRange a)
getEffectiveRange r =
    if isRangeEffective r
        then Just r
        else Nothing

fitValue :: Limitable a => LimitedRange a -> a -> a
fitValue r x
    | x < lower r = lower r
    | x > upper r = upper r
    | otherwise = x
