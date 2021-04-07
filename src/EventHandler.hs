{-# LANGUAGE OverloadedStrings #-}
module EventHandler (Event(..), SelectMode(..), dispatchAction, ErrorMessage) where

import           Animation     (FrameNum)
import qualified Animation     as A
import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (foldl')
import           Data.Maybe    (fromJust, mapMaybe)
import qualified Data.Text     as T
import qualified Joint         as J
import           JointSelect   as Sel
import qualified Tree          as T

data SelectMode = Set | Toggle
data Event
    -- Joint operations
    = CreateJoint Int Int
    | TrySelect Int Int SelectMode
    | RotateSelected Double
    | MoveSelected Double Double
    | ExtendSelectionRect Double Double
    | DragRotateSelected Double Double
    -- Animation
    | CreateFrame
    | DeleteFrame
    | ShowFrame FrameNum

type ErrorMessage = T.Text

dispatchAction :: ST.AppState -> Event -> Either ErrorMessage ST.AppState
dispatchAction s e =
    let animation = ST.animation s
        body = A.currentFrameBody $ ST.animation s
    in case e of
        CreateJoint x y ->
            let newJointId = ST.nextCreateJointId s
                parentJointId = head $ ST.selectedJointIds s
                translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = screenToLocalBody body (ST.viewScale s) translateX translateY x y
                body' = B.createJoint body parentJointId newJointId localX localY
            in Right s
                { ST.animation = A.setCurrentFrameBody animation body'
                , ST.nextCreateJointId = newJointId + 1
                }

        TrySelect x y selectMode ->
            let translateX = ST.translateX s
                translateY = ST.translateY s
                bodyOnScreen = bodyToScreenCoordinates body (ST.viewScale s) translateX translateY
            in case trySelectAt bodyOnScreen x y of
                Just jointId ->
                    let selectedJointIds = case selectMode of
                                            Set -> [jointId]
                                            Toggle -> Sel.toggle jointId (ST.selectedJointIds s)
                    in Right s { ST.selectedJointIds = selectedJointIds }
                Nothing      -> Right s -- TODO

        RotateSelected deg ->
            let rotatees = T.val <$> mapMaybe
                    (\jointId -> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body))
                    (ST.selectedJointIds s)
                rotateActions = [B.rotateJoint (ST.jointLockMode s) deg j | j <- rotatees]
                body' = foldl' (\body rotateNext -> rotateNext body) body rotateActions
            in Right s { ST.animation = A.setCurrentFrameBody animation body' }

        MoveSelected x y ->
            let translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = Sel.screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)

                jointId = head $ ST.selectedJointIds s
                joint = T.val <$> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body)
            in case joint of
                Nothing     -> Left $ "jointId " <> T.pack (show jointId) <> " not found. This should not happen."
                Just joint  ->
                    let body' = B.moveJoint localX localY joint body
                    in Right s { ST.animation = A.setCurrentFrameBody animation body' }

        ExtendSelectionRect x y -> Right s

        DragRotateSelected x y -> Right s

        CreateFrame -> Right s { ST.animation = A.appendFrame animation body }

        DeleteFrame -> Right s { ST.animation = A.deleteCurrentFrame animation }

        ShowFrame frameNum -> Right s { ST.animation = A.setCurrentFrame animation frameNum }

{- TODO:
    - deleting the last frame results in index out of bounds error
    - when creating a joint in a frame, it should be created in other frames as well
        -> first version should just place the joint in same position in all frames,
            regardless of other positions. Later on it could be possible to take into account
            parent distance in each frame or something else "intelligent".
-}
