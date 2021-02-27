module Joint where

import           Calc  (angle, distance, rotd)
import           Units (Degrees, getDegrees, getRadians, rad)

type JointId = Int

data JointLockMode
    -- Child joints will stay where they are
    = NoLock
    -- Children's distance to the rotated joint will be kept, but no rotation is applied to the children
    | DragChildren
    -- Children rotate with the rotated joint (around the common parent)
    | RotateChildren

data Joint = Joint
    { jointId       :: JointId
    , jointX        :: Double
    , jointY        :: Double
    , jointLocalRot :: Degrees -- Rotation around parent joint
    , jointWorldRot :: Degrees -- Rotation on screen
    , jointR        :: Double -- Distance (radius for rotation) to parent
    } deriving Show

instance Eq Joint where
    a == b = jointId a == jointId b

setChildAngleAndRadius :: Joint -> Joint -> Joint
setChildAngleAndRadius parent child =
    let (x, y) = (jointX child, jointY child)
        (x', y') = (jointX parent, jointY parent)
        radius = distance x' y' x y
        worldRot = angle x' y' x y
        localRot = rotd worldRot (-jointWorldRot parent)
    in child { jointLocalRot = localRot, jointWorldRot = worldRot, jointR = radius }

rotate :: JointLockMode -> Degrees -> Joint -> Joint -> Joint
rotate lockMode d parent rotatee =
    let
        -- Update angle of rotation
        localRot = rotd (jointLocalRot rotatee) d
        worldRot = rotd (jointWorldRot rotatee) d
        -- Translate based on new rotation
        x = jointX parent + (jointR rotatee * cos (getRadians . rad $ jointWorldRot rotatee))
        y = jointY parent + (jointR rotatee * sin (getRadians . rad $ jointWorldRot rotatee))
        -- dx = x - jointX rotatee
        -- dy = y - jointY rotatee
    in
        rotatee { jointX = x, jointY = y, jointLocalRot = localRot, jointWorldRot = worldRot }


