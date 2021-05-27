{-# LANGUAGE DeriveGeneric #-}
module Luukasa.Joint where

import           Calc         (angle, distance, rotd)
import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Units        (Degrees, getDegrees, getRadians, rad)

type JointId = Int

data JointLockMode
    -- Child joints will stay where they are
    = LockNone
    -- Children's distance to the rotated joint will be kept, but no rotation is applied to the children
    | LockDrag
    -- Children rotate with the rotated joint (around the common parent)
    | LockRotate
    deriving Show


data Joint = Joint
    { jointId       :: JointId
    , jointX        :: Double
    , jointY        :: Double
    , jointLocalRot :: Degrees -- ^ Rotation around parent joint
    , jointWorldRot :: Degrees -- ^ Rotation on screen
    , jointR        :: Double --  ^ Distance (radius for rotation) to parent
    } deriving (Generic, Show)

instance Eq Joint where
    a == b = jointId a == jointId b

instance ToJSON Joint
instance FromJSON Joint

{- |
Used to keep joint's geometric relation to its parent in check
after rotating or translating the parent joint.
-}
setChildAngleAndRadius
    :: Joint    -- ^ Parent
    -> Joint    -- ^ Child to be updated.
    -> Joint
setChildAngleAndRadius parent child =
    let (x, y) = (jointX child, jointY child)
        (x', y') = (jointX parent, jointY parent)
        radius = distance x' y' x y
        worldRot = angle x' y' x y
        localRot = rotd worldRot (getDegrees $ -jointWorldRot parent)
    in child { jointLocalRot = localRot, jointWorldRot = worldRot, jointR = radius }

rotate :: Double -> Joint -> Joint -> Joint
rotate deg parent rotatee =
    let
        -- Update angle of rotation
        localRot = rotd (jointLocalRot rotatee) deg
        worldRot = rotd (jointWorldRot rotatee) deg
        -- Translate based on new rotation
        x = jointX parent + (jointR rotatee * cos (getRadians . rad $ worldRot))
        y = jointY parent + (jointR rotatee * sin (getRadians . rad $ worldRot))
    in
        rotatee { jointX = x, jointY = y, jointLocalRot = localRot, jointWorldRot = worldRot }

radiusPosition :: Joint -> Joint -> (Double, Double)
radiusPosition parent child = (x, y)
  where
    x = jointX parent + (jointR child * cos (getRadians . rad $ jointWorldRot child))
    y = jointY parent + (jointR child * sin (getRadians . rad $ jointWorldRot child))
