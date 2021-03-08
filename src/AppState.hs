module AppState where

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

data AppState = AppState
    { actionState       :: ActionState
    , body              :: Body
    , nextCreateJointId :: Int
    , fileName          :: Maybe T.Text
    , viewScale         :: Double
    , viewTranslate     :: (Int, Int)
    , selectedJointIds  :: [JointId]
    , jointLockMode     :: JointLockMode
    , dragMode          :: DragMode
    } deriving (Show)


initialState :: AppState
initialState = AppState
    { actionState = Idle
    , body = B.create
    , nextCreateJointId = B.rootJointId + 1
    , selectedJointIds = []
    , fileName = Nothing
    , viewScale = 1
    , viewTranslate = (0, 0)
    , jointLockMode = Rotate
    , dragMode = DragMove
    }

selectionSize :: AppState -> Int
selectionSize = length . selectedJointIds

printJoints :: AppState -> String
printJoints s = (\j -> show j ++ "\n") <$> toList $ B.root (body s)

printState :: AppState -> String
printState s =
    "Sel: " ++ show (selectedJointIds s) ++ "\n"
    ++ "aState:" ++ show (actionState s) ++ "\n"
