module Body (Body(..), create, jointPositions, limbSegments, rootJointId, getParent, rotate, addJoint) where

import           Data.Function ((&))
import           Data.List

import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)
import           Joint         (Joint (..), JointId, JointLockMode (..))
import qualified Joint         as J
import           Tree          (Tree)
import qualified Tree          as T (children, create, findBy, insert,
                                     replaceNode, replaceVal, setChildren,
                                     setVal, val)
import           Units
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

rotate :: Body -> JointLockMode -> Double -> Joint -> Body
rotate body lockMode deg joint =
    let isRotatee j = J.jointId j == J.jointId joint
        parent = getParent body (J.jointId joint)
        -- joint = T.val $ fromJust $ T.findBy isRotatee (root body)
        originalX = J.jointX joint
        originalY = J.jointY joint

        rotated = J.rotate deg parent joint

        dx = J.jointX rotated - originalX
        dy = J.jointY rotated - originalY

        -- TODO: this whole juggling of nodes and values is bit convoluted

        -- Replace the original joint value
        withJointRotated = T.replaceVal isRotatee rotated (root body)

        -- Find node of the rotated joint
        rotatedJointNode = fromJust $ T.findBy isRotatee withJointRotated

        -- Modify children
        newChildrenTree = T.setChildren rotatedJointNode
            (rotateAdjustChild lockMode dx dy deg rotatedJointNode <$> T.children rotatedJointNode)

        in body { root = T.replaceNode (isRotatee . T.val) newChildrenTree withJointRotated }


-- TODO: holy shit this is ugly
rotateAdjustChild :: JointLockMode -> Double -> Double -> Double -> Tree Joint -> Tree Joint -> Tree Joint
rotateAdjustChild lockMode dx dy deg parentNode jointNode =
    let parent = T.val parentNode
        joint = T.val jointNode
        (jx, jy) = (J.jointX joint, J.jointY joint)
    in case lockMode of
        NoLock -> T.setVal jointNode (J.setChildAngleAndRadius parent joint)

        Drag ->
            let node = T.setVal jointNode (joint { J.jointX = jx + dx, J.jointY = jy + dy })
                children =
                    rotateAdjustChild lockMode dx dy deg jointNode <$> T.children jointNode
            in T.setChildren node children

        Rotate ->
            let parentX = J.jointX parent
                parentY = J.jointY parent
                -- TODO: it's hacky: J.rotate updates local rotation but we don't want that so
                -- the value is preserved and replaced after the call
                originalLocalRot = J.jointLocalRot joint
                joint' = J.rotate deg parent joint
                node = T.setVal jointNode joint' { J.jointLocalRot = originalLocalRot }
                children = rotateAdjustChild lockMode dx dy deg jointNode <$> T.children jointNode
            in  T.setChildren node children


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
