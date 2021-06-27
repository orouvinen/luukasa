{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LimitedRange
    ( LimitedRange
    , mkRange
    , lower
    , upper
    , setLower
    , setUpper
    , effectiveRange
    , isRangeEffective
    , fitValue)
    where

import           Data.Aeson (FromJSON, ToJSON)
newtype LimitedRange a = LimitedRange (a, a) deriving (FromJSON, ToJSON)

type Limitable a = (Ord a, Eq a)

instance (Limitable a, Show a) => Show (LimitedRange a) where
    show r = show (lower r) ++ ".." ++ show (upper r)

mkRange :: Limitable a => a -> a -> LimitedRange a
mkRange x y = LimitedRange (lower', upper')
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
lower (LimitedRange r) = fst r

upper :: Limitable a => LimitedRange a -> a
upper (LimitedRange r) = snd r

isRangeEffective :: Limitable a => LimitedRange a -> Bool
isRangeEffective r = lower r /= upper r

effectiveRange :: Limitable a => LimitedRange a -> Maybe (LimitedRange a)
effectiveRange r =
    if isRangeEffective r
        then Just r
        else Nothing

fitValue :: Limitable a => LimitedRange a -> a -> a
fitValue r x
    | x < lower r = lower r
    | x > upper r = upper r
    | otherwise = x
