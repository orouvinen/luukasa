{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

module Luukasa.AnimatorState where

import           Data.Foldable     (toList)
import           Data.Text         (Text)

import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (mapMaybe)
import           GHC.Generics      (Generic)
import           Luukasa.Animation (Animation)
import qualified Luukasa.Animation as A
import           Luukasa.Body      (Body)
import qualified Luukasa.Body      as B
import           Luukasa.Common    (TimerCallbackId, TimestampUs)
import qualified Luukasa.Data.Tree as T
import           Luukasa.Joint     (Joint, JointId, JointLockMode (..))
import qualified Luukasa.Joint     as J

data DragState = DragSelected DragMode | DragSelectionRect deriving (Generic, Show)
instance FromJSON DragState
instance ToJSON DragState

data ActionState
    = Idle
    | PlacingNewJoint
    | Drag DragState
    | AnimationPlayback TimerCallbackId
    deriving (Generic, Show)

instance FromJSON ActionState
instance ToJSON ActionState

data DragMode = DragMove | DragRotate deriving (Generic, Show)
instance FromJSON DragMode
instance ToJSON DragMode

defaultFps :: Int
defaultFps = 24

initialAnimation :: Animation Body
initialAnimation = A.appendFrame (A.mkAnimation defaultFps) B.create

data AnimatorState = AnimatorState
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
    , currentFileName   :: Maybe Text
    , jointIterLookup   :: Map Text J.JointId
    } deriving (Generic, Show)

instance FromJSON AnimatorState
instance ToJSON AnimatorState

initialAnimatorState :: AnimatorState
initialAnimatorState = AnimatorState
    { actionState = Idle
    , animation = initialAnimation
    , nextCreateJointId = B.rootJointId + 1
    , selectedJointIds = []
    , fileName = Nothing
    , viewScale = 1
    , translateX = 0
    , translateY = 0
    , jointLockMode = LockNone
    , dragMode = DragRotate
    , frameStart = Nothing
    , currentFileName = Nothing
    , jointIterLookup = Map.empty
    }

isPlaybackOn :: AnimatorState -> Bool
isPlaybackOn s = case actionState s of
    AnimationPlayback _ -> True
    _                   -> False

selectionSize :: AnimatorState -> Int
selectionSize = length . selectedJointIds

visibleBody :: AnimatorState -> Body
visibleBody = A.currentFrameData . animation

setVisibleBody :: AnimatorState -> Body -> AnimatorState
setVisibleBody s b =
    s { animation = A.setCurrentFrameData (animation s) b }

selectedJoint :: AnimatorState -> Maybe Joint
selectedJoint s@(selectionSize -> 1) =
    T.val <$> T.findNodeBy (\j -> J.jointId j == selectedJointId) body
  where
    body = B.root $ A.currentFrameData $ animation s
    selectedJointId = head $ selectedJointIds s
selectedJoint (selectionSize -> _) = Nothing

selectedNonRootJoints :: AnimatorState -> [Joint]
selectedNonRootJoints s =
    let body = A.currentFrameData $ animation s
    in T.val <$> mapMaybe
        (\jointId ->
            if jointId == B.rootJointId
                then Nothing
                else T.findNodeBy (\j -> J.jointId j == jointId) (B.root body))
        (selectedJointIds s)

printJoints :: AnimatorState -> String
printJoints s = (\j -> show j ++ "\n") <$> toList $ B.root body
  where
    body = A.currentFrameData (animation s)

printState :: AnimatorState -> String
printState s =
    "animation:" ++ show (animation s) ++ "\n"
