module EventHandler (Event(..), SelectMode(..), dispatchAction) where

import qualified AppState      as ST
import qualified Body          as B
import           Data.Foldable (foldl')
import           Data.Function ((&))
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

        TrySelect x y selectMode ->
            let translateX = fromIntegral . fst $ ST.viewTranslate s
                translateY = fromIntegral . snd $ ST.viewTranslate s
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
                    (\jointId -> T.findBy (\j -> J.jointId j == jointId) (B.root body))
                    (ST.selectedJointIds s)
                rotateActions = [B.rotate (ST.jointLockMode s) deg j | j <- rotatees]
            -- Nope, I don't think this is the meaning of "notational convenience" in the documentation for &.
            -- I know it'd be more readable to write out the lambda for fold explicitly, but this is my hobby
            -- project and I need to have some fun right now goddamnit.
            -- (To clarify, you'd be looking at "(\x f -> f x)" in place of "&")
            in s { ST.body = foldl' (&) body rotateActions }

        MoveSelected x y -> s

        ExtendSelectionRect x y -> s

        DragRotateSelected x y -> s

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
