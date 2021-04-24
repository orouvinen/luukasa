module Luukasa.AppState where

import           Data.Foldable     (toList)
import qualified Data.Text         as T

import           Luukasa.Animation (Animation)
import qualified Luukasa.Animation as A
import           Luukasa.Body      (Body)
import qualified Luukasa.Body      as B
import           Luukasa.Joint     (JointId, JointLockMode (..))

data ActionState
    = Idle
    | PlacingNewJoint
    | DragSelected DragMode
    | DragSelectionRect
    | AnimationPlayBack
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
    , fileName          :: Maybe T.Text
    , viewScale         :: Double
    , translateX        :: Double
    , translateY        :: Double
    , selectedJointIds  :: [JointId]
    , jointLockMode     :: JointLockMode
    , dragMode          :: DragMode
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
    , jointLockMode = Rotate
    , dragMode = DragMove
    }

selectionSize :: AppState -> Int
selectionSize = length . selectedJointIds

visibleBody :: AppState -> Body
visibleBody = A.currentFrameData . animation

setVisibleBody :: AppState -> Body -> AppState
setVisibleBody s b =
    s { animation = A.setCurrentFrameData (animation s) b }

printJoints :: AppState -> String
printJoints s = (\j -> show j ++ "\n") <$> toList $ B.root body
  where
    body = A.currentFrameData (animation s)

printState :: AppState -> String
printState s =
    "animation:" ++ show (animation s) ++ "\n"
