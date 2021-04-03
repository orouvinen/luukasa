module EventHandler (Event(..), SelectMode(..), dispatchAction) where

import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (foldl')
import           Data.Maybe    (fromJust, mapMaybe)
import qualified Joint         as J
import           JointSelect   as Sel
import qualified Tree          as T

data SelectMode = Set | Toggle
data Event
    = CreateJoint Int Int
    | TrySelect Int Int SelectMode
    | RotateSelected Double
    | MoveSelected Double Double
    | ExtendSelectionRect Double Double
    | DragRotateSelected Double Double

dispatchAction :: ST.AppState -> Event -> ST.AppState
dispatchAction s e =
    let body = ST.visibleBody s
    in case e of
        CreateJoint x y ->
            let newJointId = ST.nextCreateJointId s
                parentJointId = head $ ST.selectedJointIds s
                translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = screenToLocalBody body (ST.viewScale s) translateX translateY x y
                body' = createJoint body parentJointId newJointId localX localY
            in (ST.setVisibleBody s body') { ST.nextCreateJointId = newJointId + 1 }

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
            in ST.setVisibleBody s body'

        MoveSelected x y ->
            let translateX = ST.translateX s
                translateY = ST.translateY s
                (localX, localY) = Sel.screenToLocal (ST.viewScale s) translateX translateY (truncate x) (truncate y)

                jointId = head $ ST.selectedJointIds s
                joint = T.val $ fromJust $ T.findNodeBy (\j -> J.jointId j == jointId) (B.root body)
                body' = B.moveJoint localX localY joint body
                in ST.setVisibleBody s body'

        ExtendSelectionRect x y -> s

        DragRotateSelected x y -> s

createJoint :: B.Body -> J.JointId -> J.JointId -> Double -> Double -> B.Body
createJoint body parentJointId jointId x y =
    let parent = T.val $ fromJust $ T.findNodeBy (\j -> J.jointId j == parentJointId) (B.root body)
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
    in B.addJoint body parent newJoint
