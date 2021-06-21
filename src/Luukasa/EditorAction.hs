{-# LANGUAGE OverloadedStrings #-}
module Luukasa.EditorAction (Action(..), SelectMode(..), dispatchAction, ErrorMessage, ActionResult) where

import           Data.Foldable       (foldl')
import           Data.Function       ((&))
import qualified Data.Text           as T
import           Luukasa.Common

import qualified Luukasa.Animation   as A
import qualified Luukasa.AppState    as ST
import qualified Luukasa.Body        as B
import qualified Luukasa.Joint       as J
import           Luukasa.JointSelect as Sel
import qualified Tree                as T

data SelectMode = Set | Toggle
data Action
    = CreateJoint Int Int
    | DeleteSelected
    | TrySelect Int Int SelectMode
    | RotateSelected Double
    | MoveSelected Double Double
    | ExtendSelectionRect Double Double
    | DragRotateSelected Double Double
    | LevelSelectedRadiusesToMin
    | LevelSelectedRadiusesToMax
    | CreateFrame
    | DeleteFrame
    | FrameStep Int
    | ApplyToAnimationJointWithId J.JointId (J.Joint -> J.Joint)


type ActionResult = Either ErrorMessage ST.AppState

dispatchAction :: ST.AppState -> Action -> ActionResult
dispatchAction s e =
    case e of
        ApplyToAnimationJointWithId jointId f -> modifyAnimationJointWith s jointId f
        CreateJoint x y            -> createJoint s x y
        DeleteSelected             -> deleteSelectedJoints s
        TrySelect x y selectMode   -> trySelect s x y selectMode
        RotateSelected deg         -> rotateSelected s deg
        MoveSelected x y           -> moveSelected s x y
        ExtendSelectionRect x y    -> Right s
        DragRotateSelected x y     -> rotateSelectedTowards s x y
        CreateFrame                -> createFrame s
        DeleteFrame                -> deleteFrame s
        FrameStep n                -> frameStep s n
        LevelSelectedRadiusesToMin -> levelSelectedRadiuses s minimum
        LevelSelectedRadiusesToMax -> levelSelectedRadiuses s maximum

modifyAnimationJointWith :: ST.AppState -> J.JointId -> (J.Joint -> J.Joint) -> ActionResult
modifyAnimationJointWith s jointId f =
    Right s { ST.animation = fmap (B.applyToJoint jointId f) (ST.animation s)}


createJoint :: ST.AppState -> Int -> Int -> ActionResult
createJoint s x y =
    let animation = ST.animation s
        body = ST.visibleBody s
        newJointId = ST.nextCreateJointId s
        parentJointId = head $ ST.selectedJointIds s
        (translateX, translateY) = (ST.translateX s, ST.translateY s)
        (localX, localY) = screenToLocalBody body (ST.viewScale s) translateX translateY x y
        animation' = fmap (B.createJoint parentJointId newJointId localX localY) animation
    in Right s
        { ST.animation = animation'
        , ST.nextCreateJointId = newJointId + 1
        }

deleteSelectedJoints :: ST.AppState -> ActionResult
deleteSelectedJoints s =
    let animation = ST.animation s
        jointIds = J.jointId <$> ST.selectedNonRootJoints s
        deleteActions = [B.deleteJoint j | j <- jointIds]
        applyDelete = \b -> foldl' (&) b deleteActions
    in Right s { ST.animation = applyDelete <$> animation }

trySelect :: ST.AppState -> Int -> Int -> SelectMode -> ActionResult
trySelect s x y selectMode =
    let body = A.currentFrameData $ ST.animation s
        translateX = ST.translateX s
        translateY = ST.translateY s
        bodyOnScreen = bodyToScreenCoordinates body (ST.viewScale s) translateX translateY
    in case trySelectAt bodyOnScreen x y of
        Nothing      -> Right s
        Just jointId ->
            let selectedJointIds = case selectMode of
                                    Set -> [jointId]
                                    Toggle -> Sel.toggle jointId (ST.selectedJointIds s)
            in Right s { ST.selectedJointIds = selectedJointIds }

rotateSelected :: ST.AppState -> Double -> ActionResult
rotateSelected s deg = Right
    $ withCurrentFrameSelectedJoints s
    $ B.rotateJoint (ST.jointLockMode s) deg

rotateSelectedTowards :: ST.AppState -> Double -> Double -> ActionResult
rotateSelectedTowards s x y =
    let (translateX, translateY) = (ST.translateX s, ST.translateY s)
        (localX, localY) = screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)
    in Right
        $ withCurrentFrameSelectedJoints s
        $ B.rotateJointTowards localX localY (ST.jointLockMode s)

moveSelected :: ST.AppState -> Double -> Double -> ActionResult
moveSelected s x y =
    let animation = ST.animation s
        body = A.currentFrameData animation
        translateX = ST.translateX s
        translateY = ST.translateY s
        (localX, localY) = Sel.screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)

        jointId = head $ ST.selectedJointIds s
        joint = T.val <$> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body)
    in case joint of
        Nothing -> Left $ "jointId " <> T.pack (show jointId) <> " not found. This should not happen."
        Just j  ->
            let body' = B.setJointPosition localX localY j body
            in Right s { ST.animation = A.setCurrentFrameData animation body' }

createFrame :: ST.AppState -> ActionResult
createFrame s =
    let body = ST.visibleBody s
        animation = ST.animation s
    in Right s { ST.animation = A.appendFrame animation body }

deleteFrame :: ST.AppState -> ActionResult
deleteFrame s = Right s { ST.animation = A.deleteCurrentFrame (ST.animation s)}

frameStep :: ST.AppState -> Int -> ActionResult
frameStep s n = Right s { ST.animation = A.frameStep (ST.animation s) n }

levelSelectedRadiuses :: ST.AppState -> ([Double] -> Double) -> ActionResult
levelSelectedRadiuses s selectRadius =
    let radius = selectRadius (J.jointR <$> ST.selectedNonRootJoints s)
    in Right
        $ withCurrentFrameSelectedJoints s
        $ B.setJointRadius radius



{- |
    withCurrentFrameSelectedJoints applies a function `f` to all selected joints in the current frame.
    `f` is is given a joint and the body it belongs to, and should return new body with the modified joint.

    New AppState with the modified frame data (body) is returned.
-}
withCurrentFrameSelectedJoints :: ST.AppState -> (J.Joint -> B.Body -> B.Body) -> ST.AppState
withCurrentFrameSelectedJoints st f =
    let animation = ST.animation st
        js = ST.selectedNonRootJoints st
        body = A.currentFrameData animation
        actions = [f j | j <- js]
        body' = foldl' (&) body actions
    in st { ST.animation = A.setCurrentFrameData animation body' }


