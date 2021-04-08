module Frame where
import           Data.Text (Text)
import qualified Data.Text as T

import           Body
data Frame = Frame
    { _num  :: Int
    , _body :: Body
    }

instance Show Frame where
    show f = "FrameNum " ++ show (num f)

mkFrame :: Int -> Int -> Body -> Frame
mkFrame fps num body = Frame { _num = num, _body = body }

num :: Frame -> Int
num = _num

body :: Frame -> Body
body = _body

setBody :: Frame -> Body -> Frame
setBody f b = f { _body = b }
