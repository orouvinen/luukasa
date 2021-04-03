module AppState where

import           Animation     (Animation)
import qualified Animation     as A

import           Body          (Body)
import qualified Body          as B
import           Data.Foldable (toList)
import qualified Data.Text     as T
import           Joint         (JointId, JointLockMode (..))

data ActionState
    = Idle
    | PlacingNewJoint
    | DragSelected DragMode
    | DragSelectionRect
    | AnimationPlayBack
    deriving Show

data DragMode = DragMove | DragRotate deriving Show

defaultFps :: Int
defaultFps = 25

initialAnimation :: Animation
initialAnimation = A.appendFrame (A.mkAnimation defaultFps) B.create

data AppState = AppState
    { actionState       :: ActionState
    , animation         :: Animation
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
visibleBody = A.currentFrameBody . animation

setVisibleBody :: AppState -> Body -> AppState
setVisibleBody s b =
    s { animation = A.setCurrentFrameBody (animation s) b }

printJoints :: AppState -> String
printJoints s = (\j -> show j ++ "\n") <$> toList $ B.root body
  where
    body = A.currentFrameBody (animation s)

printState :: AppState -> String
printState s =
    "Sel: " ++ show (selectedJointIds s) ++ "\n"
    ++ "aState:" ++ show (actionState s) ++ "\n"
