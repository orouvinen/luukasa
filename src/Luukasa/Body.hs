{-# LANGUAGE DeriveGeneric #-}

module Luukasa.Body
    ( Body(..)
    , create
    , deleteJoint
    , isRoot
    , getParentUnsafe
    , jointPositions
    , limbSegments
    , rootJointId
    , rotateJointTowards
    , rotateJoint
    , setJointRadius
    , addJoint
    , setJointPosition
    , createJoint)
    where

import           Data.Function ((&))
import           Data.List     (foldl')
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust)
import           Luukasa.Joint (Joint, JointId, JointLockMode (..))
import qualified Luukasa.Joint as J
import           Tree          (Tree)
import qualified Tree          as T (children, create, delete, findNode,
                                     findNodeBy, insert, replaceNode,
                                     replaceNodeBy, replaceVal, replaceValBy,
                                     setChildValues, setChildren, setVal, val)

import           Calc          (angle)
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Functor  ((<&>))
import           GHC.Generics  (Generic)
import qualified Units

rootJointId :: JointId
rootJointId = 0

type ParentLookup = Map J.JointId J.JointId

data Body = Body
    { root         :: Tree Joint
    , parentLookup :: ParentLookup
    , translateX   :: Int
    , translateY   :: Int
    } deriving (Generic, Show)

instance ToJSON Body
instance FromJSON Body

create :: Body
create =
    let rootJoint = J.Joint
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

rotateJointTowards :: Double -> Double -> JointLockMode -> Joint -> Body -> Body
rotateJointTowards x y lockMode joint body =
    let parent = getParentUnsafe body (J.jointId joint)
        a1 = angle (J.jointX parent) (J.jointY parent) (J.jointX joint) (J.jointY joint)
        a2 = angle (J.jointX parent) (J.jointY parent) x y
        delta = Units.getDegrees $ a2 - a1
    in rotateJoint lockMode delta joint body

rotateJoint :: JointLockMode -> Double -> Joint -> Body -> Body
rotateJoint lockMode deg joint body =
    let parent = getParentUnsafe body (J.jointId joint)
        originalX = J.jointX joint
        originalY = J.jointY joint

        rotated = J.rotate deg parent joint

        dx = J.jointX rotated - originalX
        dy = J.jointY rotated - originalY

        -- Replace the original joint value
        root' = T.replaceVal joint rotated (root body)

        rotatedJointNode = fromJust $ T.findNode joint root'

        -- Cascade effects of rotation onto children of the rotated joint
        nodeWithUpdatedChildren = T.setChildren rotatedJointNode
            (rotateAdjustChild lockMode dx dy deg rotatedJointNode <$> T.children rotatedJointNode)

        in body { root = T.replaceNode rotatedJointNode nodeWithUpdatedChildren root' }

rotateAdjustChild :: JointLockMode -> Double -> Double -> Double -> Tree Joint -> Tree Joint -> Tree Joint
rotateAdjustChild lockMode dx dy deg parentNode jointNode =
    let parent = T.val parentNode
        joint = T.val jointNode
        (jx, jy) = (J.jointX joint, J.jointY joint)
    in case lockMode of
        LockNone -> T.setVal jointNode (J.setChildAngleAndRadius parent joint)

        LockDrag ->
            let node = T.setVal jointNode (joint { J.jointX = jx + dx, J.jointY = jy + dy })
                children =
                    rotateAdjustChild lockMode dx dy deg jointNode <$> T.children jointNode
            in T.setChildren node children

        LockRotate ->
            -- Follow parent's rotation but restore local rotation as it has not changed
            let originalLocalRot = J.jointLocalRot joint
                joint' = J.rotate deg parent joint
                node = T.setVal jointNode joint' { J.jointLocalRot = originalLocalRot }

                children = rotateAdjustChild lockMode dx dy deg node <$> T.children node
            in  T.setChildren node children


setJointRadius :: Double -> Joint -> Body -> Body
setJointRadius r joint body =
    let j = joint { J.jointR = r }
        mbParent = getParent body (J.jointId j)
    in case mbParent of
        Nothing -> body
        Just parent ->
            let (x, y) = J.radiusPosition parent j
            in replaceJoint j (j { J.jointX = x, J.jointY = y }) body



setJointPosition :: Double -> Double -> Joint -> Body -> Body
setJointPosition x y joint body =
    let parent = getParentUnsafe body (J.jointId joint)
        jointNode = fromJust $ T.findNode joint (root body)
        children = T.children jointNode

        translatedJoint = joint { J.jointX = x, J.jointY = y }
        newJoint = if isRoot joint
                   then translatedJoint
                   else J.setChildAngleAndRadius parent translatedJoint

        updatedChildren = J.setChildAngleAndRadius newJoint <$> (T.val <$> children)
        nodeWithUpdatedChildren = T.setChildValues jointNode updatedChildren
        root' = T.replaceNodeBy (\n -> J.jointId (T.val n) == J.jointId joint) nodeWithUpdatedChildren (root body)

    in body
        { root = T.replaceValBy
                    (\j -> J.jointId j == J.jointId joint)
                    newJoint
                    root'
        }


isRoot :: Joint -> Bool
isRoot j = J.jointId j == rootJointId

addJoint :: Body -> Joint -> Joint -> Body
addJoint body parent joint = body
    { root = T.insert joint (\j -> J.jointId j == J.jointId parent) (root body)
    , parentLookup = Map.insert (J.jointId joint) (J.jointId parent) (parentLookup body)
    }

replaceJoint :: Joint -> Joint -> Body -> Body
replaceJoint old new body  =
    body { root = T.replaceVal old new (root body)}

getParentUnsafe :: Body -> JointId -> Joint
getParentUnsafe body jointId =
    let parentId = parentLookup body ! jointId
        parentJoint = fromJust $ T.findNodeBy (\j -> J.jointId j == parentId) (root body)
        in T.val parentJoint

getJointById :: Body -> JointId -> Maybe Joint
getJointById body jointId =
    let node = T.findNodeBy (\j -> J.jointId j == jointId) (root body)
    in T.val <$> node

jointPositions :: Body -> [(Double, Double)]
jointPositions body =
    foldl' (\coords j -> (J.jointX j, J.jointY j) : coords) [] (root body)

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
        x = J.jointX . T.val
        y = J.jointY . T.val

createJoint :: J.JointId -> J.JointId -> Double -> Double -> Body -> Body
createJoint parentJointId jointId x y body =
    let parent = T.val $ fromJust $ T.findNodeBy (\j -> J.jointId j == parentJointId) (root body)
        newJoint =
            J.setChildAngleAndRadius
                parent
                (J.Joint
                    { J.jointX = x
                    , J.jointY = y
                    , J.jointId = jointId
                    , J.jointLocalRot = 0
                    , J.jointWorldRot = 0
                    , J.jointR = 0
                    })
    in addJoint body parent newJoint

{-
    Joint deletion is slightly involved.
    When deleting a joint, we need to:
        1. delete the given joint
        2. update parent lookup table
        3. update deleted joint's children's geometric relation to their parent,
           which has now changed.
-}
deleteJoint :: J.JointId -> Body -> Body
deleteJoint jointId body =
    let body' = body { root = T.delete (\x -> J.jointId (T.val x) == jointId) (root body) }
    -- Ids of deleted joint's children
        childIds =
            filter (\kvp -> snd kvp == jointId)
            (Map.toList (parentLookup body))
            <&> fst

    -- New parent id for the said children
        newParentId = parentLookup body ! jointId

    -- Update parent lookup. The parent of the deleted joint inherits any children the deleted joint might have.
        parentLookup' = parentLookupUpdateForInherit childIds newParentId (parentLookup body)

        body'' = foldl' (\b childId -> updateChildGeometry b newParentId childId) body' childIds
    in body'' { parentLookup = parentLookup' }

parentLookupUpdateForInherit :: [J.JointId] -> J.JointId -> ParentLookup -> ParentLookup
parentLookupUpdateForInherit childIds newParentId parentLookup' =
    foldl' (\m childId -> Map.adjust (const newParentId) childId m)
    parentLookup' childIds

getParent :: Body -> JointId -> Maybe Joint
getParent b childId = Map.lookup childId (parentLookup b) >>= getJointById b

updateChildGeometry :: Body -> J.JointId -> J.JointId -> Body
updateChildGeometry body parentId childId =
    let parent = getJointById body parentId
        child = getJointById body childId
        updatedChild = J.setChildAngleAndRadius <$> parent <*> child
    in case updatedChild of
        Nothing -> body
        Just j -> body { root = T.replaceVal (fromJust child) j (root body) }
