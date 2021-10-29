module Luukasa.Event.Keyboard where

import           Data.Word (Word32)
import qualified GI.Gdk    as Gdk

class Monad m => KeyEvent m where
    getKey :: Gdk.EventKey -> m Word32
