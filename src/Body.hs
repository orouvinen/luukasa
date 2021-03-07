module Body (Body(..), create, jointPositions, limbSegments, rootJointId, getParent, rotate, addJoint) where

import           Data.Function ((&))
import           Data.List

import           Calc
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
    , parentLookup :: Map JointId JointId
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


rotate :: JointLockMode -> Double -> Joint -> Body -> Body
rotate lockMode deg joint body =
    let isRotatee j = J.jointId j == J.jointId joint
        parent = getParent body (J.jointId joint)
        originalX = J.jointX joint
        originalY = J.jointY joint

        rotated = J.rotate deg parent joint

        dx = J.jointX rotated - originalX
        dy = J.jointY rotated - originalY

        -- Replace the original joint value
        body' = T.replaceVal isRotatee rotated (root body)

        -- Find node of the rotated joint in the new tree
        rotatedJointNode = fromJust $ T.findBy isRotatee body'

        -- Cascade effects of rotation onto children of the rotated joint
        nodeWithUpdatedChildren = T.setChildren rotatedJointNode
            (rotateAdjustChild lockMode dx dy deg rotatedJointNode <$> T.children rotatedJointNode)

        in body { root = T.replaceNode (isRotatee . T.val) nodeWithUpdatedChildren body' }

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
            -- Follow parent's rotation but restore local rotation as it has not changed
            let originalLocalRot = J.jointLocalRot joint
                joint' = J.rotate deg parent joint
                node = T.setVal jointNode joint' { J.jointLocalRot = originalLocalRot }

                children = rotateAdjustChild lockMode dx dy deg node <$> T.children node
            in  T.setChildren node children


-- TODO: this can only really work on single selected joint
rotateTowards :: JointLockMode -> Double -> Double -> Tree Joint -> Body -> Body
rotateTowards lockMode x y jointNode body =
    let joint = T.val jointNode
        parent = getParent body (J.jointId joint)
        (px, py) = (J.jointX parent, J.jointY parent)
        (jx, jy) = (J.jointX joint, J.jointY joint)
        a1 = angle px py jx jy
        a2 = angle px py x y
        delta = getDegrees $ a2 - a1
    in rotate lockMode delta joint body


addJoint :: Body -> Joint -> Joint -> Body
addJoint body parent joint = body
    { root = T.insert joint (\j -> J.jointId j == J.jointId parent) (root body)
    , parentLookup = Map.insert (J.jointId joint) (J.jointId parent) (parentLookup body)
    }

-- TODO:this is unsafe in more than one way. (`!` and `fromJust`)
getParent :: Body -> JointId -> Joint
getParent body jointId =
    let parentId = parentLookup body ! jointId
        parentJoint = fromJust $ T.findBy (\j -> J.jointId j == parentId) (root body)
        in T.val parentJoint

jointPositions :: Body -> [(Double, Double)]
jointPositions body =
    foldl' (\coords j -> (jointX j, jointY j) : coords) [] (root body)

limbSegments :: Body -> [((Double, Double), (Double, Double))]
limbSegments body = segmentsToChildren $ root body

segmentsToChildren :: Tree Joint -> [((Double, Double), (Double, Double))]
segmentsToChildren from =
    let children = T.children from
        segmentToChild = segmentBetween from
        ownChildren = segmentToChild <$> children
        restOfBody = concatMap segmentsToChildren children
    in
        ownChildren ++ restOfBody

segmentBetween :: Tree Joint -> Tree Joint -> ((Double, Double), (Double, Double))
segmentBetween from to = ((from & x, from & y), (to & x, to & y))
    where
        x = jointX . T.val
        y = jointY . T.val
