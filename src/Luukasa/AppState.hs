{-# LANGUAGE ViewPatterns #-}

module Luukasa.AppState where

import           Data.Foldable     (toList)
import           Data.Text         (Text)

import           Data.Maybe        (mapMaybe)
import           Luukasa.Animation (Animation)
import qualified Luukasa.Animation as A
import           Luukasa.Body      (Body)
import qualified Luukasa.Body      as B
import           Luukasa.Common
import           Luukasa.Joint     (Joint, JointId, JointLockMode (..))
import qualified Luukasa.Joint     as J
import qualified Tree              as T

data DragState = DragSelected DragMode | DragSelectionRect deriving Show
data ActionState
    = Idle
    | PlacingNewJoint
    | Drag DragState
    | AnimationPlayback TimerCallbackId
    deriving Show

data DragMode = DragMove | DragRotate deriving Show

defaultFps :: Int
defaultFps = 24

initialAnimation :: Animation Body
initialAnimation = A.appendFrame (A.mkAnimation defaultFps) B.create

class Monad m => HasAppState m where
    get :: m AppState
    put :: AppState -> m ()

data AppState = AppState
    { actionState       :: ActionState
    , animation         :: Animation Body
    , nextCreateJointId :: Int
    , fileName          :: Maybe Text
    , viewScale         :: Double
    , translateX        :: Double
    , translateY        :: Double
    , selectedJointIds  :: [JointId]
    , jointLockMode     :: JointLockMode
    , dragMode          :: DragMode
    , frameStart        :: Maybe TimestampUs
    } deriving (Show)

initialState :: AppState
initialState = AppState
    { actionState = Idle
    , animation = initialAnimation
    , nextCreateJointId = B.rootJointId + 1
    , selectedJointIds = []
    , fileName = Nothing
    , viewScale = 1
    , translateX = 0
    , translateY = 0
    , jointLockMode = NoLock
    , dragMode = DragRotate
    , frameStart = Nothing
    }

isPlaybackOn :: AppState -> Bool
isPlaybackOn s = case actionState s of
    AnimationPlayback _ -> True
    _                   -> False

selectionSize :: AppState -> Int
selectionSize = length . selectedJointIds

visibleBody :: AppState -> Body
visibleBody = A.currentFrameData . animation

setVisibleBody :: AppState -> Body -> AppState
setVisibleBody s b =
    s { animation = A.setCurrentFrameData (animation s) b }

selectedJoint :: AppState -> Maybe Joint
selectedJoint s@(selectionSize -> 1) =
    T.val <$> T.findNodeBy (\j -> J.jointId j == selectedJointId) body
  where
    body = B.root $ A.currentFrameData $ animation s
    selectedJointId = head $ selectedJointIds s
selectedJoint (selectionSize -> _) = Nothing

selectedNonRootJoints :: AppState -> [Joint]
selectedNonRootJoints s =
    let body = A.currentFrameData $ animation s
    in T.val <$> mapMaybe
        (\jointId ->
            if jointId == B.rootJointId
                then Nothing
                else T.findNodeBy (\j -> J.jointId j == jointId) (B.root body))
        (selectedJointIds s)

printJoints :: AppState -> String
printJoints s = (\j -> show j ++ "\n") <$> toList $ B.root body
  where
    body = A.currentFrameData (animation s)

printState :: AppState -> String
printState s =
    "animation:" ++ show (animation s) ++ "\n"
