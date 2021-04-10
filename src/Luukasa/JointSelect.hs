module Luukasa.JointSelect
    ( trySelectAt
    , bodyToScreenCoordinates
    , screenToLocal
    , screenToLocalBody
    , toggle
    , isSelected)
    where

import           Calc
import           Data.Foldable (toList)
import qualified Luukasa.Body  as B
import qualified Luukasa.Joint as J

selectRadius :: Double
selectRadius = 5.0

trySelectAt :: B.Body -> Int -> Int -> Maybe J.JointId
trySelectAt body x y =
    let jointsNearEnough = [j | j <- toList $ B.root body, isInSelectRadius j]
        isInSelectRadius j =
                distance (J.jointX j) (J.jointY j) (fromIntegral x) (fromIntegral y)
            <= selectRadius
    in if null jointsNearEnough
        then Nothing
        else Just $ J.jointId $ head jointsNearEnough

isSelected :: J.JointId -> [J.JointId] -> Bool
isSelected = elem

toggle :: J.JointId -> [J.JointId] -> [J.JointId]
toggle jointId selection =
    if isSelected jointId selection
        then filter (/= jointId) selection
        else jointId : selection


-- TODO: the below conversions surely don't belong in this module

-- | Convert body to screen coordinates
bodyToScreenCoordinates :: B.Body -> Double -> Double -> Double -> B.Body
bodyToScreenCoordinates body scaleFactor trX trY = body
    { B.root = fmap
        (\j ->
            let (screenX, screenY) =
                    localToScreen scaleFactor trX trY (J.jointX j) (J.jointY j)
            in j { J.jointX = fromIntegral . toInteger $ screenX
                 , J.jointY = fromIntegral . toInteger $ screenY
                 }
        ) (B.root body)
    }

-- |Screen coordinates to local conversion, taking into account body translation
screenToLocalBody :: B.Body -> Double -> Double -> Double -> Int -> Int -> (Double, Double)
screenToLocalBody body scaleFactor trX trY x y =
    screenToLocal scaleFactor trX trY (x + B.translateX body) (y + B.translateY body)

-- |Local coordinates to screen conversion, taking into account body translation
localToScreenBody :: B.Body -> Double -> Double -> Double -> Double -> Double -> (Int, Int)
localToScreenBody body scaleFactor trX trY x y =
    let bodyTranslateX = fromIntegral $ B.translateX body
        bodyTranslateY = fromIntegral $ B.translateY body
    in localToScreen scaleFactor trX trY (x + bodyTranslateX) (y + bodyTranslateY)

-- |Screen coordinates to local conversion
screenToLocal :: Double -> Double -> Double -> Int -> Int -> (Double, Double)
screenToLocal scaleFactor trX trY x y =
    let localX = (fromIntegral x - trX) / scaleFactor
        localY = (fromIntegral y - trY) / scaleFactor
    in (localX, localY)

-- |Local coordinates to screen conversion
localToScreen :: Double -> Double -> Double -> Double -> Double -> (Int, Int)
localToScreen scaleFactor trX trY x y =
    let screenX = (x * scaleFactor) + trX
        screenY = (y * scaleFactor) + trY
    in (truncate screenX, truncate screenY)
