module Body (Body(..), create, jointPositions, limbSegments, rootJointId, getParent, addJoint) where

import           Data.Function ((&))
import           Data.List

import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Joint         (Joint (..), JointId)
import qualified Joint         as J
import           Tree          (Tree)
import qualified Tree          as T
rootJointId :: JointId
rootJointId = 0

data Body = Body
    { root         :: Tree Joint
    , parentLookup :: Map JointId Joint
    , translateX   :: Int
    , translateY   :: Int
    } deriving Show

create :: Body
create =
    let rootJoint = Joint
            { J.jointId = rootJointId
            , J.jointX = 0
            , J.jointY = 0
            , J.jointLocalRot = 0
            , J.jointWorldRot = 0
            , J.jointR = 0
            }
    in Body
        { root = T.create rootJoint
        , translateX = 0
        , translateY = 0
        , parentLookup = Map.empty
        }

addJoint :: Body -> Joint -> Joint -> Body
addJoint body parent joint = body
    { root = T.insert joint (\j -> J.jointId j == J.jointId parent) (root body)
    , parentLookup = Map.insert (J.jointId joint) parent (parentLookup body)
    }

-- TODO: in theory, this is unsafe (`!` calls error when key doesn't exist)
getParent :: Body -> JointId -> Joint
getParent body jointId = parentLookup body ! jointId

jointPositions :: Body -> [(Double, Double)]
jointPositions body =
    foldl' (\coords j -> (jointX j, jointY j) : coords) [] (root body)

limbSegments :: Body -> [((Double, Double), (Double, Double))]
limbSegments body = segmentsToChildren $ root body

segmentsToChildren :: Tree Joint -> [((Double, Double), (Double, Double))]
segmentsToChildren from =
    let f           = segmentBetween from
        ownChildren = fmap f (T.children from)
        restOfBody  = concatMap segmentsToChildren (T.children from)
    in
        ownChildren ++ restOfBody

segmentBetween :: Tree Joint -> Tree Joint -> ((Double, Double), (Double, Double))
segmentBetween from to = ((from & x, from & y), (to & x, to & y))
    where
        x = jointX . T.val
        y = jointY . T.val
