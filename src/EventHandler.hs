module EventHandler (Event(..), dispatchAction) where

import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (toList)
import           Data.Maybe    (fromJust)
import qualified Joint         as J
import           JointSelect
import qualified Tree          as T
import           Units         (Degrees, getDegrees)

data Event
    = CreateJoint Int Int
    | TrySelect Int Int
    | Rotate J.JointId Double

dispatchAction :: ST.AppState -> Event -> ST.AppState
dispatchAction s e =
    let body = ST.body s
    in case e of
        CreateJoint x y ->
            let newJointId = ST.nextCreateJointId s
                parentJointId = head $ ST.selectedJointIds s
                translateX = fromIntegral . fst $ ST.viewTranslate s
                translateY = fromIntegral . snd $ ST.viewTranslate s
                (localX, localY) = screenToLocalBody body (ST.viewScale s) translateX translateY x y
            in
                s
                { ST.body = createJoint body parentJointId newJointId localX localY
                , ST.nextCreateJointId = newJointId + 1
                }

        TrySelect x y ->
            let translateX = fromIntegral . fst $ ST.viewTranslate s
                translateY = fromIntegral . snd $ ST.viewTranslate s
                bodyOnScreen = bodyToScreenCoordinates body (ST.viewScale s) translateX translateY
            in case trySelectAt bodyOnScreen x y of
                Just jointId -> s { ST.selectedJointIds = [jointId]}
                Nothing      -> s

        Rotate jointId deg ->
            let rotatedJointNode = T.findBy (\j -> J.jointId j == jointId) (B.root body)
            in case rotatedJointNode of
                Nothing -> s
                Just node ->
                    let joint = T.val node
                    in s { ST.body = B.rotate body (ST.jointLockMode s) deg joint }

createJoint :: B.Body -> J.JointId -> J.JointId -> Double -> Double -> B.Body
createJoint body parentJointId jointId x y =
    let parent = T.val $ fromJust $ T.findBy (\j -> J.jointId j == parentJointId) (B.root body) -- TODO: something something meh fromJust something
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
