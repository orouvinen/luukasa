module EventHandler (Event(..), SelectMode(..), dispatchAction) where

import qualified Animation     as A
import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (foldl')
import           Data.Maybe    (fromJust, mapMaybe)
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
    | ShowFrame Int

dispatchAction :: ST.AppState -> Event -> ST.AppState
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
            in s
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
                    in s { ST.selectedJointIds = selectedJointIds }
                Nothing      -> s

        RotateSelected deg ->
            let rotatees = T.val <$> mapMaybe
                    (\jointId -> T.findNodeBy (\j -> J.jointId j == jointId) (B.root body))
                    (ST.selectedJointIds s)
                rotateActions = [B.rotateJoint (ST.jointLockMode s) deg j | j <- rotatees]
                body' = foldl' (\body rotateNext -> rotateNext body) body rotateActions
            in s { ST.animation = A.setCurrentFrameBody animation body' }

        MoveSelected x y ->
            let translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = Sel.screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)

                jointId = head $ ST.selectedJointIds s
                joint = T.val $ fromJust $ T.findNodeBy (\j -> J.jointId j == jointId) (B.root body)
                body' = B.moveJoint localX localY joint body
            in s { ST.animation = A.setCurrentFrameBody animation body' }

        ExtendSelectionRect x y -> s

        DragRotateSelected x y -> s

        CreateFrame ->
            let body' = A.currentFrameBody animation
            in s { ST.animation = A.appendFrame animation body' }

        DeleteFrame -> s

        ShowFrame frameNum -> s { ST.animation = A.setCurrentFrame animation frameNum }
