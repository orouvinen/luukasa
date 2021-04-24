{-# LANGUAGE OverloadedStrings #-}
module Luukasa.EventHandler (Event(..), SelectMode(..), dispatchAction, ErrorMessage) where

import           Data.Foldable       (foldl')
import           Data.Function       ((&))
import           Data.Maybe          (mapMaybe)
import qualified Data.Text           as T

import qualified Luukasa.Animation   as A
import qualified Luukasa.AppState    as ST
import qualified Luukasa.Body        as B
import qualified Luukasa.Joint       as J
import           Luukasa.JointSelect as Sel
import qualified Tree                as T

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
    | FrameStep Int

-- TODO: define this elsewhere. Main has to import it from here and ideally Main doesn't import anything from this layer.
type ErrorMessage = T.Text

dispatchAction :: ST.AppState -> Event -> Either ErrorMessage ST.AppState
dispatchAction s e =
    let animation = ST.animation s
        body = A.currentFrameData $ ST.animation s
    in case e of
        CreateJoint x y ->
            let newJointId = ST.nextCreateJointId s
                parentJointId = head $ ST.selectedJointIds s
                (translateX, translateY) = (ST.translateX s, ST.translateY s)
                (localX, localY) = screenToLocalBody body (ST.viewScale s) translateX translateY x y
                animation' = fmap (B.createJoint parentJointId newJointId localX localY) animation
            in Right s
                { ST.animation = animation'
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
            let nonRootJoints = filter (/= B.rootJointId) (ST.selectedJointIds s)
                rotatees = T.val <$> mapMaybe
                    (\jointId -> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body))
                    nonRootJoints
                rotateActions = [B.rotateJoint (ST.jointLockMode s) deg j | j <- rotatees]
                body' = foldl' (&) body rotateActions
            in Right s { ST.animation = A.setCurrentFrameData animation body' }

        MoveSelected x y ->
            let translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = Sel.screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)

                jointId = head $ ST.selectedJointIds s
                joint = T.val <$> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body)
            in case joint of
                Nothing     -> Left $ "jointId " <> T.pack (show jointId) <> " not found. This should not happen."
                Just j  ->
                    let body' = B.moveJoint localX localY j body
                    in Right s { ST.animation = A.setCurrentFrameData animation body' }

        ExtendSelectionRect x y -> Right s

        DragRotateSelected x y -> Right s

        CreateFrame -> Right s { ST.animation = A.appendFrame animation body }

        DeleteFrame -> Right s { ST.animation = A.deleteCurrentFrame animation }

        FrameStep n -> Right s { ST.animation = A.frameStep animation n }

